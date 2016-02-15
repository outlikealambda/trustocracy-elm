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


import Session exposing (Session)
import Topic.Model exposing (Topic)
import Topic.View
import ActiveUser exposing (ActiveUser(LoggedIn, LoggedOut))
import Login
import Navigate


import Routes exposing (Route)
import TransitRouter
import TransitStyle


type alias Model = TransitRouter.WithRoute Routes.Route
  { session : Session
  , topics : List Topic
  , login : Login.Model
  }


type Action
  = LoginMsg Login.Action
  | LoginSuccess
  | SessionMsg Session.Action
  | TopicsLoad (List Topic)
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
      -- since User and Topic live inside Session, we handle a topic update
      -- by updating the Session
      , updateSession <| Session.GoCompose topicId
      )

    Routes.Connect topicId ->
      ( model
      , updateSession <| Session.GoConnect topicId
      )

    -- map to [] if fail, since this will probably be the
    -- home page and we don't want to continually redirect
    Routes.Topics ->
      ( model
      , Topic.Model.fetchAll
        |> Task.toMaybe
        |> Task.map (Maybe.withDefault [])
        |> Task.map TopicsLoad
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
  let
    (model, fx) =
      TransitRouter.init routerConfig path initialModel

  in
    ( model
    , Effects.batch
      [ fx
      , updateSession <| Session.SetUser (ActiveUser.toUser activeUser)])


initialModel : Model
initialModel =
  { transitRouter = TransitRouter.empty Routes.EmptyRoute
  , topics = []
  , login = Login.init
  , session = Session.init
  }


update : Action -> Model -> (Model, Effects Action)
update message model =
  case message of
    SessionMsg sessionAction ->
      let
        (update, updateFx) =
          Session.update sessionAction model.session
      in
        ( { model | session = update }
        , Effects.map SessionMsg updateFx
        )

    LoginMsg msg ->
      let
        (loginModel, fx, worldAction) =
          Login.update loginContext (Debug.log "Login msg" msg) model.login
      in
        ( { model | login = loginModel }
        , Effects.map worldAction fx )

    LoginSuccess ->
        ( model
        , Effects.batch
          [ goHome
          , Signal.send ActiveUser.save (Login.getUser model.login)
            |> Effects.task
            |> Effects.map (\_ -> NoOp)
          ]
        )

    RouterAction routeAction ->
      TransitRouter.update routerConfig routeAction model

    TopicsLoad topics ->
      ( { model | topics = topics }
      , Effects.none )

    NoOp ->
      ( model
      , Effects.none
      )

    LoadUserState activeUser ->
      case activeUser of

        LoggedIn user ->
          ( model
          , updateSession <| Session.SetUser user
          )

        LoggedOut ->
          ( model
          , Effects.batch
            [ updateSession <| Session.ClearUser
            , goHome
            ]
          )


loginContext : Login.Context Action
loginContext =
  Login.Context
    LoginMsg
    (\_ -> LoginSuccess)


updateSession : Session.Action -> Effects Action
updateSession sessionAction =
  Task.succeed sessionAction
    |> Task.map SessionMsg
    |> Effects.task


view : Signal.Address Action -> Model -> Html
view address model =

  div [ class "world container" ]
    [ Navigate.view model.session.user
    , div
      [ class "content" ]
      [ case TransitRouter.getRoute model of

        Routes.Topics ->
          div
            [ style (TransitStyle.fadeSlideLeft 1000 (TransitRouter.getTransition model)) ]
            [ Topic.View.viewAll model.topics ]

        Routes.Home ->
          div []
            [ Login.view (Signal.forwardTo address LoginMsg) model.login ]

        Routes.Connect _ ->
          Session.view (Signal.forwardTo address SessionMsg)
            <| model.session

        Routes.Compose _ ->
          Session.view (Signal.forwardTo address SessionMsg) model.session

        Routes.EmptyRoute ->
          text ""
      ]
    ]


goHome : Effects Action
goHome =
  Effects.map (\_ -> NoOp) (Routes.redirect Routes.Topics)
