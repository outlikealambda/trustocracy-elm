module Auth
  ( Auth
  , Action
    ( Show )
  , SignalContext
  , signal
  , Context
  , init
  , update
  , viewHeader
  , viewForm
  , getUser
  ) where


import String
import Task
import Html exposing (Html, h2, div, text, input, button, a)
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
  | Logout
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
  , login : (User -> a)
  , logout : () -> a
  }


update : Context a -> Action -> Auth -> (Auth, Effects a)
update context message auth =
  case message of

    -- could extract this into a : String -> Maybe InputId
    UpdateInput rawInput ->
      case rawInput of

        "" ->
          ( { auth | input = Empty }
          , Effects.none
          )

        raw ->
          case String.toInt raw of

            Err _ ->
              ( auth
              , Effects.none
              )

            Ok inputInt ->
              ( { auth | input = UserId inputInt }
              , Effects.none
              )

    LoadUser ->
      let fx =
        case auth.input of

          (UserId userId) ->
            API.fetchUser userId ValidateUser

          Empty ->
            Effects.none
      in
        ( auth
        , Effects.map context.next fx
        )

    ValidateUser maybeUser ->
      case Debug.log "maybeUser" maybeUser of

        -- could just set the property on the model here?
        Nothing ->
          ( { auth
            | message = "nope, please try again"
            , input = Empty }
          , Effects.none
          )

        Just user ->
          let
            activeUser = ActiveUser.LoggedIn user
          in
            ( { auth
              | activeUser = activeUser
              , visible = False
              }
            , Task.succeed user
              |> Effects.task
              |> Effects.map context.login
          )

    Show ->
      ( { auth | visible = True }
      , Effects.none )

    Logout ->
      ( { auth
        | visible = True
        , input = Empty
        , activeUser = ActiveUser.LoggedOut
        }
      , Task.succeed ()
        |> Effects.task
        |> Effects.map context.logout
      )

    FacebookAuth maybeAuthResponse ->
      let
        fx = Maybe.map (API.fetchUserByFacebookAuth ValidateUser) maybeAuthResponse
          |> Maybe.withDefault (Effects.task (Task.succeed (ValidateUser Nothing)))
      in
        ( auth
        , Effects.map context.next fx
        )


viewHeader : Signal.Address Action -> Auth -> Html
viewHeader address {activeUser}=
  let
    (userName, login) =
      case activeUser of
        ActiveUser.LoggedOut ->
          ( []
          , [ div [ class "login" ]
              [ a
                [ onClick address Show ]
                [ text "login"]
              ]
            ]
          )
        ActiveUser.LoggedIn user ->
          ( [ div
              [ class "user" ][ text user.name ] ]
          , [ div
              [ class "logout" ]
              [ a
                [ onClick address Logout ]
                [ text "logout"]
              ]
            ]
          )
  in
    div
      [ class "auth-header" ]
      <| login
      ++ userName




viewForm : Signal.Address Action -> Auth -> Html
viewForm address auth =
  let
    currentInput =
      case auth.input of
        Empty -> ""
        UserId userId -> toString userId
    toggleClass =
      if auth.visible then "login-form visible" else "login-form"

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
