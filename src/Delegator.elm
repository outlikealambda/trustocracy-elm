module Delegator exposing
  ( Delegator
  , fromActiveUser
  , Msg
  , update
  , view
  , navHeader
  )


import Html as Html exposing (Html, div, text, p, span, a)
import Html.Attributes as Attribute exposing (class)
import Html.Events as Event exposing (onClick)
import Json.Decode as Json
import String
import Task exposing (Task)


import ActiveUser exposing (ActiveUser (LoggedIn, LoggedOut))
import Common.API as API
import Common.Form as Form
import Common.Relationship as Relationship exposing (Relationship)
import Location
import Auth.Google as Google
import Routes
import Trustee exposing (Trustee)


type alias Delegator =
  { saved : List Trustee
  , current : List Trustee
  , errors : List String
  , input : String
  }


type Msg
  = NoOp
  | ValidateMove Trustee
  | SaveDelegateComplete (List Trustee)
  | InputUpdate String
  | Lookup
  | LookupComplete (Maybe Trustee)
  | RequestGoogleContacts
  | SetPath Routes.Route


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


update : Msg -> Delegator -> (Delegator, Cmd Msg)
update message delegator =
  case message of

    NoOp ->
      ( delegator, Cmd.none )

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
            , Cmd.none
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
                  Cmd.none
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
        , Cmd.none
        )

    InputUpdate input ->
      ( { delegator | input = String.toLower(input) }
      , Cmd.none
      )

    Lookup ->
      ( delegator
      , API.lookupTrustee LookupComplete delegator.input
      )

    LookupComplete maybeTrustee ->
      case maybeTrustee of
        Nothing ->
          ( { delegator
            | errors =
              ("Sorry, we couldn't find anyone with the email " ++ delegator.input)
              :: delegator.errors
            }

          , Cmd.none )
        Just trustee ->
          let
            isNew =
              not <| List.any (Trustee.isTrustee trustee) delegator.current
            (fx, error) =
              -- if it's a new person, let's save them as a candidate
              if isNew then
                ( { trustee | relationship = Relationship.Candidate }
                  |> Task.succeed
                  |> Task.perform (\_ -> NoOp) ValidateMove
                , ""
                )
              else
                ( Cmd.none
                , trustee.name ++ " is already linked to you :)"
                )
          in
            ( { delegator
              | input = ""
              , errors = error :: delegator.errors }
            , fx
            )

    RequestGoogleContacts ->
      ( delegator, Google.requestContacts )

    SetPath route ->
      ( delegator, Location.setPath <| Routes.encode route )


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


view : List String -> Delegator -> Html Msg
view emails {current, errors, input} =
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
        [ viewDelegates Relationship.Candidate candidates ]

  in
    div
      [ class "delegator" ]
      <| [ section "Organize Your People"
           <| [ viewDelegates Relationship.Bff bffs ]
           ++ [ viewDelegates Relationship.Trusted trusted ]
           ++ candidateView
           ++ (viewErrors errors)
         ]
      ++ [ section "Find More People"
           <| [ emailLookup input ]
           ++ [ googleContacts ]
          ]
      ++ [ section "Get Found" [ viewEmails emails ] ]


viewEmails : List String -> Html msg
viewEmails emails =
  let
    emailLi address =
      Html.li
        [ class "email" ]
        [ Html.text address ]
  in
    div
      [ class "user-emails" ]
      [ delegateSubHeader
        "Your Emails"
        <| Just "Other users will be able to follow you if they know (exactly) any of your following email addresses"
      , Html.ul
        [ class "emails" ]
        (List.map emailLi emails)
      ]


viewErrors : List String -> List (Html msg)
viewErrors errors =
  if List.isEmpty errors then
    []
  else
    [ div
      [ class "delegate-errors" ]
      [ div
        [ class "delegate-header delegate-title" ]
        [ Html.text "Whoops!" ]
      , Html.ul
         []
         (List.map viewError errors)
      ]
    ]


viewError : String -> Html msg
viewError error =
  Html.li
    [ class "error" ]
    [ Html.text error ]


emailLookup : String -> Html Msg
emailLookup current =
  div
    [ class "email-lookup" ]
    [ delegateSubHeader
      "Email Address"
      <| Just "Put in an email address and we'll check if they're in our system.  Exact matches only, but uppercase/lowercase doesn't matter."
    , Html.input
      [ Form.onEnter (\_ -> Lookup)
      , Attribute.placeholder "bob@gmail.com"
      , Attribute.value <| current
      , Event.on "input" (Json.map InputUpdate Event.targetValue)
      ]
      []
    ]


viewDelegates : Relationship -> List Trustee -> Html Msg
viewDelegates r trusted =
  let
    trusteeWrapper trustee =
      div
        [ class <| "delegate cf" ]
        <| Html.text trustee.name
        :: (moveButtons trustee)
  in
    div
      [ class <| "delegates " ++ relationshipClass r ]
      <| organizeHeader r
      :: List.map trusteeWrapper trusted


organizeHeader : Relationship -> Html msg
organizeHeader r =
  let
    (headerText, subtitleText) =
      case r of
        Relationship.Bff ->
          ( "Bffs"
          , "You trust and respect this group so deeply that when they hold an opinion, you value it higher than your own. "
            ++ "If they offer advice, you take it as the gospel truth."
          )
        Relationship.Trusted ->
          ( "Trusted Friends"
          , "You are always happy to at least listen to what this group has to say, even if sometimes you disagree. "
            ++ "They've rarely led you astray."
          )
        Relationship.Public ->
          ("Public Figures", "10 max")
        Relationship.Candidate ->
          ( "Persons of Interest"
          , "People you've previously followed, have looked up via email, or discovered through Google contacts or Facebook"
          )
        _ ->
          ("Who's this?", "")
  in
    delegateSubHeader headerText <| Just subtitleText


section : String -> List (Html msg) -> Html msg
section title pieces =
  div
    [ class "section" ]
    <| sectionHeader title :: pieces


sectionHeader : String -> Html msg
sectionHeader words =
  div
    [ class "delegate-header section-header" ]
    [ Html.text words ]


delegateSubHeader : String -> Maybe String -> Html msg
delegateSubHeader titleText maybeSubtitleText =
  let
    title =
      [ div
        [ class "delegate-title" ]
        [ Html.text titleText ]
      ]
    subtitle =
      case maybeSubtitleText of
        Nothing ->
          []
        Just s ->
          [ div
            [ class "delegate-subtitle" ]
            [ Html.text s ]
          ]
  in
    div
      [ class "delegate-header" ]
      <| title ++ subtitle

moveButtons : Trustee -> List (Html Msg)
moveButtons trustee =
  let
    bb =
      delegateButton Relationship.Bff trustee
    tb =
      delegateButton Relationship.Trusted trustee
    cb =
      delegateButton Relationship.Candidate trustee

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


delegateButton : Relationship -> Trustee -> Direction -> Html Msg
delegateButton relationship trustee direction =
  let
    updatedTrustee =
      { trustee | relationship = relationship }
  in
    Html.button
      [ class <| buttonClass relationship direction
      , onClick <| ValidateMove updatedTrustee
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
        , "btn-sm" ]
        ++ iconClasses direction


googleContacts : Html Msg
googleContacts =
  div
    [ class "ga-contacts" ]
    [ delegateSubHeader
      "Google Contacts"
      -- TODO: add link to privacy/conduct policy
      -- also, write privacy/conduct policy
      <| Just
        <| "See if any of your Google contacts are part of the club. "
        ++ "We'll be searching for users with matching email addresses, so we'll need your permission (Google will ask you, after you click) to read your contacts. "
        ++ "We promise not to do anything sketchy with that info. "
    , Html.button
      [ class "btn-med"
      , onClick RequestGoogleContacts
      ]
      [ text "Check Contacts" ]
    ]



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


navHeader : (Msg -> msg) -> ActiveUser -> List (Html msg)
navHeader transform activeUser =
  case activeUser of
    LoggedOut ->
      []

    LoggedIn user ->
      [ div
        [class "home" ]
        [ a
          (Routes.clickTo (transform << SetPath) Routes.UserDelegates)
          [ text <| "You trust " ++ (toString <| List.length user.trustees) ++ " people" ]
        ]
      ]
