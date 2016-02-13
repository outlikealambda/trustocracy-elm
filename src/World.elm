module World
  ( Action
  , init
  , view
  , actions
  , update) where


import String
import Html exposing (Html, input, div, node, h1, text)
import Effects exposing (Effects)
import Html.Attributes exposing (class, rel, href, placeholder, value, style)
import Html.Events exposing (on, targetValue)
import Task


import Opinion.Connector as Connector
import Opinion.Composer as Composer
import User exposing (User)
import Topic exposing (Topic)
import ActiveUser exposing (ActiveUser(LoggedIn, LoggedOut))
import Login
import Header
import Routes exposing (Route)


import TransitRouter
import TransitStyle


type alias Model = TransitRouter.WithRoute Routes.Route
  { user: User
  , topic: Topic
  , connector : Connector.Model
  , composer : Composer.Model
  , login : Login.Model
  }


type Action
  = LoginMsg Login.Action
  | LoginSuccess
  | ConnectorLoad (Maybe Topic)
  | ConnectorMsg Connector.Action
  | ComposerLoad (Maybe Topic)
  | ComposerMsg Composer.Action
  | RouterAction (TransitRouter.Action Routes.Route)
  | LoadUserState ActiveUser
  | NoOp


actions : Signal Action
actions =
  -- use mergeMany if you have other mailboxes or signals to feed into StartApp
  Signal.merge
    (Signal.map LoadUserState ActiveUser.updates)
    (Signal.map RouterAction TransitRouter.actions)


mountRoute : Route -> Route -> Model -> (Model, Effects Action)
mountRoute prevRoute route model =
  case route of

    Routes.Home ->
      ( model, Effects.none )

    Routes.Compose topicId ->
      ( model
      , Topic.get topicId
          |> Task.toMaybe
          |> Task.map ComposerLoad
          |> Effects.task
      )

    Routes.Connect topicId ->
      ( model
      , Topic.get topicId -- Task Error Topic
         |> Task.toMaybe -- Task Never (Maybe Topic)
         |> Task.map ConnectorLoad
         |> Effects.task
      )

    Routes.EmptyRoute ->
      ( model, Effects.none )


routerConfig : TransitRouter.Config Route Action Model
routerConfig =
  { mountRoute = mountRoute
  , getDurations = \_ _ _ -> (50, 200)
  , actionWrapper = RouterAction
  , routeDecoder = Routes.decode
  }


init : String -> ActiveUser -> (Model, Effects Action)
init path activeUser =
  TransitRouter.init routerConfig path (initialModel activeUser)


initialModel : ActiveUser -> Model
initialModel activeUser =
  { transitRouter = TransitRouter.empty Routes.EmptyRoute
  , user =
      case activeUser of
        LoggedIn user -> Debug.log "init logged in" user
        LoggedOut -> Debug.log "init logged out" User.empty
  , topic = Topic.empty
  , connector = Connector.empty
  , composer = Composer.empty
  , login = Login.init
  }


update : Action -> Model -> (Model, Effects Action)
update message model =
  case message of

    LoginMsg msg ->
      let
        (loginModel, fx, worldAction) =
          Login.update loginContext (Debug.log "Login msg" msg) model.login
      in
        ( { model | login = loginModel }
        , Debug.log "Login msg fx" (Effects.map worldAction fx) )

    LoginSuccess ->
        ( { model
          | user = Debug.log "Success Update" (Login.getUser model.login)
          , login = Login.init
          }
        , Effects.batch
          [ Effects.map (\_ -> NoOp) (Routes.redirect (Routes.Connect 0))
          , Signal.send ActiveUser.save (Login.getUser model.login)
            |> Effects.task
            |> Effects.map (\_ -> NoOp)
          ]
        )

    RouterAction routeAction ->
      TransitRouter.update routerConfig routeAction model

    ConnectorLoad maybeTopic ->
      case maybeTopic of
        Nothing ->
          ( model, goHome )

        Just topic ->
          let
            (connector, fx) =
              Connector.init model.user topic
          in
            ( { model | connector = connector }
            , Effects.map ConnectorMsg fx
            )

    ConnectorMsg msg ->
      let
        (connector, fx) =
          Connector.update msg model.connector
      in
        ( { model | connector = connector }
        , Effects.map ConnectorMsg fx
        )

    ComposerLoad maybeTopic ->
      case maybeTopic of
        Nothing ->
          ( model, goHome )

        Just topic ->
          let
            (composer, fx) =
              Composer.init model.user topic
          in
            ( { model | composer = composer }
            , Effects.map ComposerMsg fx
            )

    ComposerMsg msg ->
      let
        (composer, fx) =
          Composer.update msg model.composer
      in
        ( { model | composer = composer }
        , Effects.map ComposerMsg fx
        )

    NoOp ->
      ( model
      , Effects.none
      )

    LoadUserState activeUser ->
      case activeUser of

        LoggedIn user ->
          ( { model | user = Debug.log "Loaded user" user }
          , Effects.none
          )

        LoggedOut ->
          ( { model | user = Debug.log "Logging out user" User.empty }
          , goHome
          )

loginContext : Login.Context Action
loginContext =
  Login.Context
    LoginMsg
    (\_ -> LoginSuccess)


view : Signal.Address Action -> Model -> Html
view address model =

  div [ class "world container" ]
    [ Header.view model.user
    , div
      [ class "content"
      , style (TransitStyle.fadeSlideLeft 100 (TransitRouter.getTransition model))
      ]
      [ case (TransitRouter.getRoute model) of

        Routes.Home ->
          div []
          [ Login.view (Signal.forwardTo address LoginMsg) model.login
          ]

        Routes.Connect _ ->
          div [ class "row" ]
          (Connector.view (Signal.forwardTo address ConnectorMsg) model.connector)

        Routes.Compose _ ->
          Composer.view (Signal.forwardTo address ComposerMsg) model.composer

        Routes.EmptyRoute ->
          text ""
      ]
    ]

goHome : Effects Action
goHome =
  Effects.map (\_ -> NoOp) (Routes.redirect Routes.Home)
