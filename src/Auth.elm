module Auth
  ( Auth
  , Action
  , SignalContext
  , signal
  , Context
  , init
  , update
  , view
  , logoutSignal
  ) where


import Effects exposing (Effects)
import Html exposing (Html, h2, div, text, input, button, a)
import Html.Attributes as Attribute exposing (placeholder, value, class)
import Html.Events exposing (on, targetValue, onClick)
import String
import Task exposing (Task)


import Common.API as API
import Common.Form as Form
import ActiveUser exposing (ActiveUser)
import User exposing (User)
import Auth.Facebook as Facebook
import Auth.Google as Google


type alias Auth =
  { message : String
  , input : InputId
  , visible : Bool
  }


init : (Auth, Effects Action)
init =
  ( { message = "Welcome, please enter your user id"
    , input = Empty
    , visible = False
    }
  , API.checkForActiveUser ValidateUser
  )


type InputId
  = Empty
  | UserCreds String String


type Action
  = UpdateInput String String
  | ValidateUser (Maybe User)
  | SetVisible Bool
  | Logout
  | LoadUser
  | FacebookAuth (Maybe Facebook.AuthResponse)
  | GoogleAuth (Maybe Google.AuthResponse)


type alias SignalContext =
  { facebook : Signal (Maybe Facebook.AuthResponse)
  , google : Signal (Maybe Google.AuthResponse)
  }


signal : SignalContext -> Signal Action
signal signalContext =
  Signal.mergeMany
    [ Signal.map FacebookAuth signalContext.facebook
    , Signal.map GoogleAuth signalContext.google
    ]


type alias Context a =
  { next : (Action -> a)
  , setUser : (ActiveUser -> a)
  }


update : Context a -> Action -> Auth -> (Auth, Effects a)
update context message auth =
  case message of

    -- could extract this into a : String -> Maybe InputId
    UpdateInput name secret ->
      case name of

        "" ->
          ( { auth | input = Empty }
          , Effects.none
          )

        _ ->
          ( { auth | input = UserCreds name secret }
          , Effects.none
          )

    LoadUser ->
      let fx =
        case auth.input of

          UserCreds name secret ->
            API.loginUser (name, secret) ValidateUser

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
          ( { auth | message = "nope, please try again" }
          , Effects.none
          )

        Just user ->
          ( { auth
            | visible = False
            , input = Empty
            }
          , saveUser user
            |> Effects.task
            |> Effects.map context.setUser
          )

    SetVisible isVisible ->
      ( { auth | visible = isVisible }
      , Effects.none )

    Logout ->
      ( { auth
        | input = Empty
        }
      , clearUser
        |> Effects.task
        |> Effects.map context.setUser
      )

    FacebookAuth maybeAuthResponse ->
      let
        fx = Maybe.map (API.fetchUserByFacebookAuth ValidateUser) maybeAuthResponse
          |> Maybe.withDefault (Effects.task (Task.succeed (ValidateUser Nothing)))
      in
        ( auth
        , Effects.map context.next fx
        )

    GoogleAuth maybeAuthResponse ->
      let
        fx =
          case maybeAuthResponse of
            Nothing ->
              Task.succeed (ValidateUser Nothing) |> Effects.task
            Just gaResponse ->
              if String.contains "contacts.read" gaResponse.scope then
                API.updateGoogleContacts ValidateUser (Debug.log "gaContacts" gaResponse)
              else
                API.fetchUserByGoogleAuth ValidateUser (Debug.log "gaLogin" gaResponse)
        in
          ( auth
          , Effects.map context.next fx
          )


clearUser : Task x ActiveUser
clearUser =
  Task.map3 (\_ _ _ -> ActiveUser.LoggedOut)
    (Signal.send mailbox.address ())
    (Signal.send Facebook.address Facebook.Logout)
    (Signal.send Google.address Google.Logout)


saveUser : User -> Task x ActiveUser
saveUser user =
  Task.succeed (ActiveUser.LoggedIn user)


type alias ViewContext =
  { activeUser : ActiveUser
  , address : Signal.Address Action
  }


view : ViewContext -> Auth -> List Html
view context auth =
  [ viewHeader context
  , viewForm context.address auth
  ]


viewHeader : ViewContext -> Html
viewHeader {address, activeUser}=
  let
    (userName, login) =
      case activeUser of
        ActiveUser.LoggedOut ->
          ( []
          , [ div [ class "login" ]
              [ a
                [ onClick address (SetVisible True) ]
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
    (name, secret) =
      case auth.input of
        Empty ->
          ("", "")
        UserCreds n s ->
          (n, s)

    toggleClass =
      if auth.visible then "login-form visible" else "login-form"

  in
    div
      [ class toggleClass ]
      [ div []
        [ h2 [] [ text <| "Login" ]
        , text <| "Eventually this will be a login; for now just input the id of the user you'd like to impersonate"
        , div
          [ Form.onEnter address LoadUser ]
          [ input
            [ placeholder "User Name"
            , value <| name
            , on "input" targetValue (\n -> Signal.message address (UpdateInput n secret))
            ]
            []
          , input
            [ placeholder "Password"
            , Attribute.type' "password"
            , value <| secret
            , on "input" targetValue (\s -> Signal.message address (UpdateInput name s))
            ]
            []
          ]
        , button
          [ class "fb-login"
          , onClick Facebook.address Facebook.Login
          ]
          [ text "FB Login" ]
        , button
          [ class "ga-login"
          , onClick Google.address Google.Login
          ]
          [ text "GA Login" ]
        , button
          [ class "cancel-login"
          , onClick address (SetVisible False)
          ]
          [ text "Cancel" ]
        ]
      ]




-- FOR LOGOUT PURPOSES (clear jwt cookie)
mailbox : Signal.Mailbox ()
mailbox =
  Signal.mailbox ()


logoutSignal : Signal ()
logoutSignal =
  mailbox.signal
