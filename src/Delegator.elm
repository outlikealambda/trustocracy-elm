module Delegator
  ( Delegator
  , fromActiveUser
  , Action
  , update
  , view
  , navHeader
  ) where


import Effects exposing (Effects)
import Html as Html exposing (Html, div, text, p, span, a)
import Html.Attributes as Attribute exposing (class)
import Html.Events as Event exposing (onClick)
import String
import Task exposing (Task)


import ActiveUser exposing (ActiveUser (LoggedIn, LoggedOut))
import Common.API as API
import Common.Form as Form
import Common.Relationship as Relationship exposing (Relationship)
import Routes
import User exposing (User)
import Trustee exposing (Trustee)


type alias Delegator =
  { saved : List Trustee
  , current : List Trustee
  , errors : List String
  , input : String
  }


type Action
  = ValidateMove Trustee
  | SaveDelegateComplete (List Trustee)
  | InputUpdate String
  | Lookup
  | LookupComplete (Maybe Trustee)


type Direction
  = Up
  | Down


fromActiveUser : ActiveUser -> Delegator
fromActiveUser activeUser =
  case activeUser of
    LoggedOut ->
      Delegator [] [] [] ""
    LoggedIn user ->
      Delegator user.trustees user.trustees [] ""


update : Action -> Delegator -> (Delegator, Effects Action)
update action delegator =
  case action of
    ValidateMove trustee ->
      let
        current =
          trustee
          :: (List.filter (not << Trustee.isTrustee trustee) delegator.current)

        updated =
          { delegator | current = current }

      in
        case isValid current of
          Err errors ->
            ( { updated | errors = errors }
            , Effects.none
            )
          Ok _ ->
            let
              newOrChanged t =
                (||)
                  (List.all (not << Trustee.isTrustee t) delegator.saved) -- new
                  (List.any (Trustee.isTrusteeChanged t) delegator.saved) -- changed

              diff =
                List.filter newOrChanged current

              fx =
                if List.isEmpty diff then
                  Effects.none
                else
                  API.setTrustees SaveDelegateComplete diff
            in
              ( { updated | errors = [] }
              , fx
              )

    SaveDelegateComplete trustees ->
      let
        -- get all the delegates which weren't saved
        unchanged =
          List.filter (\t -> List.all (not << Trustee.isTrustee t) trustees) delegator.saved

        updatedDelegateList =
          trustees ++ unchanged

      in
        ( { delegator
          | saved = updatedDelegateList
          , current = updatedDelegateList }
        , Effects.none
        )

    InputUpdate input ->
      ( { delegator | input = String.toLower(input) }
      , Effects.none
      )

    Lookup ->
      ( delegator
      , API.lookupTrustee LookupComplete delegator.input
      )

    LookupComplete maybeTrustee ->
      case maybeTrustee of
        Nothing ->
          ( delegator, Effects.none )
        Just trustee ->
          let
            isNew =
              not <| List.any (Trustee.isTrustee trustee) delegator.current
            fx =
              -- if it's a new person, let's save them as a candidate
              if isNew then
                { trustee | relationship = Relationship.Candidate }
                |> Task.succeed
                |> Task.map ValidateMove
                |> Effects.task
              else
                Effects.none
          in
            ( delegator
            , fx
            )


isValid : List Trustee -> Result (List String) ()
isValid =
  hasError
    [ tooManyOfType Relationship.Bff 5
    , tooManyOfType Relationship.Trusted 5
    ]


type alias ErrorChecker = List Trustee -> Maybe String


tooManyOfType : Relationship -> Int -> ErrorChecker
tooManyOfType relationship limit =
  let
    maybeOverLimit c =
      if limit < c then
        String.join
          " "
          [ "You have more than"
          , toString limit
          , Relationship.toReadable relationship
          , " relationships"
          ]
          |> Just
      else
        Nothing
  in
    List.filter (Trustee.isRelated relationship)
    >> List.length
    >> maybeOverLimit


hasError : List ErrorChecker -> List Trustee -> Result (List String) ()
hasError errorCheckers trustees =
  let
    errors =
      List.filterMap ((|>) trustees) errorCheckers
  in
    if List.isEmpty errors then
      Ok ()
    else
      Err errors



updateDb : Trustee -> Task x Trustee
updateDb trustee =
  Task.succeed trustee


type alias ViewContext =
  { user : User
  , address : Signal.Address Action
  }


view : Signal.Address Action -> Delegator -> Html
view address {current, errors, input} =
  let
    bffs =
      List.filter (Trustee.isRelated Relationship.Bff) current
    trusted =
      List.filter (Trustee.isRelated Relationship.Trusted) current
    candidates =
      List.filter (Trustee.isRelated Relationship.Candidate) current
    candidateView =
      if List.isEmpty candidates then
        []
      else
        [ viewDelegates Relationship.Candidate address candidates ]

  in
    div
      [ class "delegator" ]
      <| viewDelegates Relationship.Bff address bffs
      :: [ viewDelegates Relationship.Trusted address trusted ]
      ++ candidateView
      ++ [ viewErrors errors ]
      ++ [ lookupInput address input ]


viewErrors : List String -> Html
viewErrors errors =
  Html.ul
   [ class "delegate-errors" ]
   <| List.map viewError errors


viewError : String -> Html
viewError error =
  Html.li
    [ class "error" ]
    [ Html.text error ]


lookupInput : Signal.Address Action -> String -> Html
lookupInput address current =
  div
    [ class "trustee-lookup" ]
    [ div
      [ Form.onEnter address Lookup ]
      [ Html.input
        [ Attribute.placeholder "bob@gmail.com"
        , Attribute.value <| current
        , Event.on "input" Event.targetValue (Signal.message address << InputUpdate)
        ]
        []
      ]
    ]


viewDelegates : Relationship -> Signal.Address Action -> List Trustee -> Html
viewDelegates r address trusted =
  let
    trusteeWrapper trustee =
      div
        [ class <| "delegate cf" ]
        <| Html.text trustee.name
        :: (moveButtons address trustee)
  in
    div
      [ class <| "delegates " ++ relationshipClass r ]
      <| delegateHeader r
      :: List.map trusteeWrapper trusted


delegateHeader : Relationship -> Html
delegateHeader r =
  let
    headerText =
      case r of
        Relationship.Bff ->
          "Bffs"
        Relationship.Trusted ->
          "Trusted Friends"
        Relationship.Public ->
          "Public Figures"
        Relationship.Candidate ->
          "Persons of Interest"
        _ ->
          "Who's this?"
  in
    div
      [ class <| "delegate-header" ]
      [ Html.text headerText ]


moveButtons : Signal.Address Action -> Trustee -> List Html
moveButtons address trustee =
  let
    bb =
      delegateButton Relationship.Bff address trustee
    tb =
      delegateButton Relationship.Trusted address trustee
    cb =
      delegateButton Relationship.Candidate address trustee

  in
    case trustee.relationship of
      Relationship.Bff ->
        [ tb Down
        ]
      Relationship.Trusted ->
        [ cb Down
        , bb Up
        ]
      _ ->
        [ tb Up
        ]


delegateButton : Relationship -> Signal.Address Action -> Trustee -> Direction -> Html
delegateButton relationship action trustee direction =
  let
    updatedTrustee =
      { trustee | relationship = relationship }
  in
    Html.button
      [ class <| buttonClass relationship direction
      , onClick action <| ValidateMove updatedTrustee
      ]
      []


buttonClass : Relationship -> Direction -> String
buttonClass relationship direction =
  String.join " "
    <| [ relationshipClass relationship
        , case direction of
          Up ->
            "up-arrow"
          Down ->
            "down-arrow"
        ]
        ++ iconClasses direction


iconClasses : Direction -> List String
iconClasses d =
  case d of
    Up ->
      [ "fa"
      , "fa-arrow-up"
      ]
    Down ->
      [ "fa"
      , "fa-arrow-down"
      ]


relationshipClass : Relationship -> String
relationshipClass r =
  case r of
    Relationship.Bff ->
      "bff"
    Relationship.Trusted ->
      "trusted"
    Relationship.Candidate ->
      "candidate"
    _ ->
      "unknown"


navHeader : ActiveUser -> List Html
navHeader activeUser =
  case activeUser of
    LoggedOut ->
      []

    LoggedIn user ->
      [ div
        [class "home" ]
        [ a
          (Routes.clickTo Routes.UserDelegates)
          [ text <| "You trust " ++ (toString <| List.length user.trustees) ++ " people" ]
        ]
      ]
