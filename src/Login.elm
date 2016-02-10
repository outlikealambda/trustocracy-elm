module Login
  ( Model
  , Action
  , Context
  , init
  , update
  , view
  , getUser
  ) where


import String
import Task
import Http
import Html exposing (Html, h2, div, text, input)
import Html.Attributes exposing (placeholder, value, class)
import Html.Events exposing (on, targetValue, keyCode)
import Common.LabeledInput as LabeledInput
import Effects exposing (Effects)
import Json.Decode as Json


import User exposing (User)


type alias Model =
  { user : User
  , message : String
  , input : InputId
  }


init : Model
init =
  { user = User.empty
  , message = "Welcome, please enter your user id"
  , input = Empty
  }


type InputId
  = Empty
  | UserId Int


type Action
  = UpdateInput String
  | ValidateUser (Maybe User)
  | LoadUser
  | NoOp


type alias ActionMap b = (Action -> b)


type alias Context b =
  { actions : ActionMap b
  , complete : ActionMap b
  }


update : Context b -> Action -> Model -> (Model, Effects Action, ActionMap b)
update context message model =
  case message of

    -- could extract this into a : String -> Maybe InputId
    UpdateInput rawInput ->
      case rawInput of

        "" ->
          ( { model | input = Empty }
          , Effects.none
          , context.actions
          )

        raw ->
          case String.toInt raw of

            Err _ ->
              ( model
              , Effects.none
              , context.actions
              )

            Ok inputInt ->
              ( { model | input = UserId inputInt }
              , Effects.none
              , context.actions
              )

    LoadUser ->
      let fx =
        case model.input of

          (UserId userId) ->
            fetchUser userId

          Empty ->
            Effects.none
      in
        ( model
        , fx
        , context.actions
        )

    ValidateUser maybeUser ->
      case Debug.log "maybeUser" maybeUser of

        -- could just set the property on the model here?
        Nothing ->
          ( { model
            | message = "nope, please try again"
            , input = Empty }
          , Effects.none
          , context.actions
          )

        Just user ->
          ( { model | user = user }
          , NoOp
            |> Task.succeed
            |> Effects.task
          , context.complete
          )
    NoOp ->
      ( model
      , Effects.none
      , context.actions
      )


view : Signal.Address Action -> Model -> Html
view address model =
  let currentInput =
    case model.input of
      Empty -> ""
      UserId userId -> toString userId
  in
    div []
      [ h2 [] [ text <| "Login" ]
      , text <| "Eventually this will be a login; for now just input the id of the user you'd like to impersonate"
      , div []
        [ input
          [ placeholder "User Id"
          , value <| currentInput
          , on "input" targetValue (Signal.message address << UpdateInput)
          , onEnter address LoadUser
          ] []
        ]
      ]


getUser : Model -> User
getUser = .user


-- no need to user.init because the decoder maps the exact fields
fetchUser : Int -> Effects Action
fetchUser userId =
  buildUserUrl userId
    |> Http.get User.decoder
    |> Task.toMaybe
    |> Task.map ValidateUser
    |> Effects.task


buildUserUrl : Int -> String
buildUserUrl userId =
  String.concat
    [ "http://localhost:3714/api/user/"
    , toString userId
    ]


-- from the Elm Architecture tutorial
onEnter : Signal.Address a -> a -> Html.Attribute
onEnter address value =
    on "keydown"
        (Json.customDecoder keyCode is13)
        (\_ -> Signal.message address value)


is13 : Int -> Result String ()
is13 code =
    if code == 13 then
        Ok ()

    else
        Err "not the right key code"
