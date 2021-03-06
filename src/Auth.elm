module Auth exposing
  ( Auth
  , Msg
  , Context
  , subscriptions
  , init
  , update
  , viewUser
  , viewButton
  , viewForm
  )


import Html exposing (Html, h2, div, text, input, button, a)
import Html.Attributes as Attribute exposing (placeholder, value, class)
import Html.Events exposing (on, targetValue, onClick)
import String
import Utils.Cmd as CmdUtils


import Common.API as API
import Common.Form as Form
import ActiveUser exposing (ActiveUser)
import Model.User exposing (User)
import Auth.Cookie as Cookie
import Auth.Facebook as Facebook
import Auth.Google as Google


type alias Auth =
  { message : String
  , input : InputId
  , visible : Bool
  }


init : (Auth, Cmd Msg)
init =
  ( { message = "Welcome, please enter your user id"
    , input = Empty
    , visible = False
    }
  , API.checkForActiveUser InvalidUser ValidUser
  )


type InputId
  = Empty
  | UserCreds String String


type Credential
  = Facebook
  | Google


type Msg
  = UpdateInput String String
  | ValidUser User
  | InvalidUser String
  | SetVisible Bool
  | Logout
  | LoadUser
  | Login Credential
  | FacebookAuth (Maybe Facebook.AuthResponse)
  | GoogleAuth (Maybe Google.AuthResponse)


subscriptions : Sub Msg
subscriptions =
  Sub.batch
    [ Facebook.authResponses FacebookAuth
    , Google.authResponses GoogleAuth
    ]


type alias Context a =
  { next : (Msg -> a)
  , setUser : (ActiveUser -> a)
  }


update : Context a -> Msg -> Auth -> (Auth, Cmd a)
update context message auth =
  case message of

    -- could extract this into a : String -> Maybe InputId
    UpdateInput name secret ->
      case name of

        "" ->
          ( { auth | input = Empty }
          , Cmd.none
          )

        _ ->
          ( { auth | input = UserCreds name secret }
          , Cmd.none
          )

    LoadUser ->
      let fx =
        case auth.input of

          UserCreds name secret ->
            API.loginUser InvalidUser ValidUser (name, secret)

          Empty ->
            Cmd.none
      in
        ( auth
        , Cmd.map context.next fx
        )

    InvalidUser message ->
      -- could just set the property on the model here?
      ( { auth | message = message }
      , Cmd.none
      )

    ValidUser user ->
      ( { auth
        | visible = False
        , input = Empty
        }
      , saveUser context.setUser user
      )

    SetVisible isVisible ->
      ( { auth | visible = isVisible }
      , Cmd.none )


    Login credentials ->
      ( auth
      , case (Debug.log "login with creds" credentials) of
          Google ->
            Google.login

          Facebook ->
            Facebook.login
      )

    Logout ->
      ( { auth
        | input = Debug.log "Logout action" Empty
        }
      , Cmd.map context.setUser clearUser
      )

    FacebookAuth maybeAuthResponse ->
      let
        fx =
          (Maybe.map
            (API.fetchUserByFacebookAuth InvalidUser ValidUser)
            maybeAuthResponse)
          |> Maybe.withDefault Cmd.none
      in
        ( auth
        , Cmd.map context.next fx
        )

    GoogleAuth maybeAuthResponse ->
      let
        performGoogleRequest gaAuthResponse =
          if String.contains "contacts.read" (Debug.log "gaAuthResponse" gaAuthResponse).scope then
            API.updateGoogleContacts
              InvalidUser
              ValidUser
              (Debug.log "gaContacts" gaAuthResponse)
          else
            API.fetchUserByGoogleAuth InvalidUser ValidUser
              (Debug.log "gaLogin" gaAuthResponse)
        fx =
          (Maybe.map
            performGoogleRequest
            (Debug.log "maybeAuthResponse" maybeAuthResponse))
          |> Maybe.withDefault Cmd.none
      in
        ( (Debug.log "google auth" auth)
        , Cmd.map context.next fx
        )


clearUser : Cmd ActiveUser
clearUser =
  Cmd.batch
    [ Cookie.logout
    , Facebook.logout
    , Google.logout
    , CmdUtils.init ActiveUser.LoggedOut
    ]


saveUser : (ActiveUser -> a) -> User -> Cmd a
saveUser setUser user =
  CmdUtils.init <| setUser (ActiveUser.LoggedIn user)


viewHeader : ActiveUser -> Html Msg
viewHeader activeUser =
  let
    (userName, login) =
      case activeUser of
        ActiveUser.LoggedOut ->
          ( []
          , [ div [ class "login" ]
              [ a
                [ onClick (SetVisible True) ]
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
                [ onClick Logout ]
                [ text "logout"]
              ]
            ]
          )
  in
    div
      [ class "auth-header" ]
      <| login
      ++ userName


viewUser : ActiveUser -> Html a
viewUser activeUser =
  case activeUser of
    ActiveUser.LoggedOut ->
      div [ class "inactive-user" ] []
    ActiveUser.LoggedIn user ->
      div [ class "user" ] [ text user.name ]


viewButton : ActiveUser -> Html Msg
viewButton activeUser =
  case activeUser of
    ActiveUser.LoggedOut ->
      div
        [ class "login" ]
        [ a
          [ onClick (SetVisible True) ]
          [ text "login"]
        ]
    ActiveUser.LoggedIn user ->
      div
        [ class "logout" ]
        [ a
          [ onClick Logout ]
          [ text "logout"]
        ]


viewForm : Auth -> Html Msg
viewForm auth =
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
      [ div [class "user-name"]
        [ h2 []
          [ text <| "Login" ]
        , text <| "Eventually this will be a login; for now just input the id of the user you'd like to impersonate and press enter/login"
        , div
          [ Form.onEnter (\_ -> LoadUser) ]
          [ Html.map ((flip UpdateInput) secret) <| input
            [ placeholder "User Name"
            , value name
            , on "input" targetValue
            ]
            []
          , Html.map (UpdateInput name) <| input
            [ placeholder "Password"
            , Attribute.type_ "password"
            , value secret
            , on "input" targetValue
            ]
            []
          , div []
            [ button
              [ class "confirm-login"
              , onClick (LoadUser)
              ]
              [ text "Login" ]
            , button
              [ class "cancel-login"
              , onClick (SetVisible False)
              ]
              [ text "Previous Page" ]
            ]
          ]
        ]
      , div [ class "other-login" ]
        [ h2 []
          [ text "OR" ]
        , button
          [ class "fb-login"
          , onClick <| Login Facebook
          ]
          [ text "Sign in with Facebook" ]
        , button
          [ class "ga-login"
          , onClick <| Login Google
          ]
          [ text "Sign in with Google" ]
      ]
    ]
