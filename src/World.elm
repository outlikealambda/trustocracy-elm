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


import Opinion.Connector as Connector
import Opinion.Composer as Composer
import User exposing (User)
import Topic exposing (Topic)
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
  | ConnectorMsg Connector.Action
  | ComposerMsg Composer.Action
  | RouterAction (TransitRouter.Action Routes.Route)
  | NoOp


actions : Signal Action
actions =
  -- use mergeMany if you have other mailboxes or signals to feed into StartApp
  Signal.map RouterAction TransitRouter.actions


mountRoute : Route -> Route -> Model -> (Model, Effects Action)
mountRoute prevRoute route model =
  case route of

    Routes.Home ->
      ( model, Effects.none )

    Routes.Compose topicId ->
      let
        (composer, fx) =
          Composer.init model.user topicId
      in
        ( { model | composer = composer }
        , Effects.map ComposerMsg fx
        )

    Routes.Connect topicId ->
      let
        (connector, fx) =
          Connector.init model.user topicId
      in
        ( { model | connector = connector }
        , Effects.map ConnectorMsg fx
        )


routerConfig : TransitRouter.Config Route Action Model
routerConfig =
  { mountRoute = mountRoute
  , getDurations = \_ _ _ -> (50, 200)
  , actionWrapper = RouterAction
  , routeDecoder = Routes.decode
  }


init : String -> (Model, Effects Action)
init path =
  TransitRouter.init routerConfig path initialModel


initialModel : Model
initialModel =
  { transitRouter = TransitRouter.empty Routes.Home
  , user = User "" -1
  , topic = 0
  , connector = Connector.empty
  , composer = Composer.empty
  , login = Login.init
  }


update : Action -> Model -> (Model, Effects Action)
update message model =
  case Debug.log "World message" message of

    LoginMsg msg ->
      let
        (loginModel, fx, worldAction) =
          Login.update loginContext (Debug.log "Login msg" msg) model.login
      in
        ( { model | login = loginModel }
        , Debug.log "Login msg fx" (Effects.map worldAction fx) )

    LoginSuccess ->
        ( { model | user = Debug.log "Success Update" (Login.getUser model.login) }
        , Effects.map (\_ -> NoOp) (Routes.redirect (Routes.Connect model.topic))
        )

    RouterAction routeAction ->
      TransitRouter.update routerConfig routeAction model

    ConnectorMsg msg ->
      let
          (connector, fx) = Connector.update msg model.connector
      in
        ( { model | connector = connector }
        , Effects.map ConnectorMsg fx
        )

    ComposerMsg msg ->
      let
          (composer, fx) = Composer.update msg model.composer
      in
          ( { model | composer = composer }
          , Effects.map ComposerMsg fx
          )

    NoOp ->
      ( model
      , Effects.none
      )


loginContext : Login.Context Action
loginContext =
  Login.Context
    LoginMsg
    (\_ -> LoginSuccess)


view : Signal.Address Action -> Model -> Html
view address model =

  div [ class "world container" ]
    [ Header.view (Debug.log "header" model.user)
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
      ]
    ]
