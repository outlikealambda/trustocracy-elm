module World
  ( Action
  , Model
  , init
  , view
  , actions
  , update) where


import Html exposing (Html, input, div, node, h1, text)
import Effects exposing (Effects)
import Html.Attributes exposing (class, rel, href, placeholder, value, style)
import Task
import String


import Session exposing (Session)
import Topic.Model exposing (Topic)
import Topic.View
import ActiveUser exposing (ActiveUser(LoggedIn, LoggedOut))
import Login
import Header
import User exposing (User)


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
  | LogoutSuccess
  | SetUser ActiveUser
  | SessionMsg Session.Action
  | TopicsLoad (List Topic)
  | RouterAction (TransitRouter.Action Routes.Route)
  | SNoOp String


actions : Signal Action
actions =
  -- use mergeMany if you have other mailboxes or signals to feed into StartApp
  Signal.merge
    (Signal.map RouterAction TransitRouter.actions)
    (Signal.map SetUser ActiveUser.signal)


mountRoute : Route -> Route -> Model -> (Model, Effects Action)
mountRoute prevRoute route world =
  case route of

    Routes.Home ->
      ( world, Effects.none )

    Routes.Compose topicId ->
      ( world
      -- since User and Topic live inside Session, we handle a topic update
      -- by updating the Session
      , updateSession <| Session.GoCompose topicId
      )

    Routes.Connect topicId ->
      ( world
      , updateSession <| Session.GoConnect topicId
      )

    -- map to [] if fail, since this will probably be the
    -- home page and we don't want to continually redirect
    Routes.Topics ->
      ( world
      , Topic.Model.fetchAll
        |> Task.toMaybe
        |> Task.map (Maybe.withDefault [])
        |> Task.map TopicsLoad
        |> Effects.task
      )

    Routes.EmptyRoute ->
      ( world, Effects.none )


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
    (world, fx) =
      TransitRouter.init routerConfig path initialModel

  in
    ( world
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
update message world =
  case message of
    SessionMsg sessionAction ->
      let
        (update, updateFx) =
          Session.update sessionAction world.session
      in
        ( { world | session = update }
        , Effects.map SessionMsg updateFx
        )

    LoginMsg msg ->
      let
        (update, updateFx) =
          Login.update
            { next = LoginMsg , complete = (\_ -> LoginSuccess) }
            (Debug.log "Login msg" msg)
            world.login
      in
        ( { world | login = update }
        , updateFx -- the Login module uses the context to create a World.Action
        )

    LoginSuccess ->
        ( world
        , addUser <| Login.getUser world.login
        )

    LogoutSuccess ->
        ( world
        , clearUser
        )

    RouterAction routeAction ->
      TransitRouter.update routerConfig routeAction world

    TopicsLoad topics ->
      ( { world | topics = topics }
      , Effects.none )

    SNoOp str ->
      let
        _ = Debug.log "SNoOp" str
      in
        ( world
        , Effects.none
        )

    SetUser activeUser ->
      case activeUser of
        LoggedIn user ->
          ( world
          , Effects.batch
            [ updateSession <| Session.SetUser user
            , goHome
            ]
          )

        LoggedOut ->
          ( world
          , Effects.batch
            [ updateSession <| Session.ClearUser
            , goHome
            ]
          )


addUser : User -> Effects Action
addUser user =
  Signal.send ActiveUser.save user
    |> Effects.task
    |> Effects.map (\_ -> SNoOp "added user")


clearUser : Effects Action
clearUser =
  Signal.send ActiveUser.clear ()
    |> Effects.task
    |> Effects.map (\_ -> SNoOp "cleared user")



updateSession : Session.Action -> Effects Action
updateSession sessionAction =
  Task.succeed sessionAction
    |> Task.map SessionMsg
    |> Effects.task


view : Signal.Address Action -> Model -> Html
view address world =

  div [ class "world container" ]
    [ Header.view world.session.user
    , div
      [ class "content" ]
      [ case TransitRouter.getRoute world of

        Routes.Topics ->
          div
            [ style (TransitStyle.fadeSlideLeft 1000 (TransitRouter.getTransition world)) ]
            [ Topic.View.viewAll world.topics ]

        Routes.Home ->
          div []
            [ Login.view (Signal.forwardTo address LoginMsg) world.login ]

        Routes.Connect _ ->
          Session.view (Signal.forwardTo address SessionMsg)
            <| world.session

        Routes.Compose _ ->
          Session.view (Signal.forwardTo address SessionMsg) world.session

        Routes.EmptyRoute ->
          text ""
      ]
    ]


goHome : Effects Action
goHome =
  Effects.map (\_ -> SNoOp "going home") (Routes.redirect Routes.Topics)
