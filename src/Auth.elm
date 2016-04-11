module Auth
  ( Auth
  , Action
    ( Show )
  , SignalContext
  , signal
  , Context
  , init
  , update
  , view
  , getUser
  ) where


import String
import Task
import Html exposing (Html, h2, div, text, input, button)
import Html.Attributes exposing (placeholder, value, class)
import Html.Events exposing (on, targetValue, keyCode, onClick)
import Effects exposing (Effects)
import Json.Decode as Json


import Common.API as API
import ActiveUser exposing (ActiveUser)
import User exposing (User)
import Auth.Facebook as Facebook


type alias Auth =
  { activeUser : ActiveUser
  , message : String
  , input : InputId
  , visible : Bool
  }


init : ActiveUser -> Auth
init activeUser =
  { activeUser = activeUser
  , message = "Welcome, please enter your user id"
  , input = Empty
  , visible = False
  }


type InputId
  = Empty
  | UserId Int


type Action
  = UpdateInput String
  | ValidateUser (Maybe User)
  | Show
  | LoadUser
  | FacebookAuth (Maybe Facebook.AuthResponse)


type alias SignalContext =
  { facebook : Signal (Maybe Facebook.AuthResponse)
  }


signal : SignalContext -> Signal Action
signal signalContext =
  Signal.map FacebookAuth signalContext.facebook


type alias Context a =
  { next : (Action -> a)
  , complete : (User -> a)
  }


update : Context a -> Action -> Auth -> (Auth, Effects a)
update context message model =
  case message of

    -- could extract this into a : String -> Maybe InputId
    UpdateInput rawInput ->
      case rawInput of

        "" ->
          ( { model | input = Empty }
          , Effects.none
          )

        raw ->
          case String.toInt raw of

            Err _ ->
              ( model
              , Effects.none
              )

            Ok inputInt ->
              ( { model | input = UserId inputInt }
              , Effects.none
              )

    LoadUser ->
      let fx =
        case model.input of

          (UserId userId) ->
            API.fetchUser userId ValidateUser

          Empty ->
            Effects.none
      in
        ( model
        , Effects.map context.next fx
        )

    ValidateUser maybeUser ->
      case Debug.log "maybeUser" maybeUser of

        -- could just set the property on the model here?
        Nothing ->
          ( { model
            | message = "nope, please try again"
            , input = Empty }
          , Effects.none
          )

        Just user ->
          ( { model
            | activeUser = ActiveUser.LoggedIn user
            , visible = False
            }
          , Task.succeed user
            |> Effects.task
            |> Effects.map context.complete
          )

    Show ->
      ( { model | visible = Debug.log "setting visible to True" True }
      , Effects.none )

    FacebookAuth maybeAuthResponse ->
      let
        fx = Maybe.map (API.fetchUserByFacebookAuth ValidateUser) maybeAuthResponse
          |> Maybe.withDefault (Effects.task (Task.succeed (ValidateUser Nothing)))
      in
        ( model
        , Effects.map context.next fx
        )


view : Signal.Address Action -> Auth -> Html
view address model =
  let
    currentInput =
      case model.input of
        Empty -> ""
        UserId userId -> toString userId
    toggleClass =
      if model.visible then "login-form visible" else "login-form"

  in
    div
      [ class toggleClass ]
      [ div []
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
        , button [ onClick Facebook.address Facebook.Login ] [ text "FB Login" ]
        , button [ onClick Facebook.address Facebook.Logout ] [ text "FB Logout" ]
        ]
      ]


getUser : Auth -> ActiveUser
getUser = .activeUser


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