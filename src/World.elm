module World
  ( Action
  , Model
  , init
  , view
  , actions
  , update
  ) where


import Html exposing (Html, input, div, node, h1, text)
import Effects exposing (Effects)
import Html.Attributes exposing (class, rel, href, placeholder, value, style)
import Task


import Session exposing (Session)
import Topic.Model exposing (Topic)
import Topic.View
import ActiveUser exposing (ActiveUser(LoggedIn, LoggedOut))
import Auth exposing (Auth)
import Header
import User exposing (User)
import Auth.Facebook as Facebook


import Routes exposing (Route)
import TransitRouter


type alias Model = TransitRouter.WithRoute Routes.Route
  { session : Session
  , topics : List Topic
  , auth : Auth
  }


type Action
  = AuthMsg Auth.Action
  | Login User
  | Logout
  | SetUser ActiveUser
  | SessionMsg Session.Action
  | TopicsLoad (List Topic)
  | RouterAction (TransitRouter.Action Routes.Route)
  | SNoOp String


actions : Auth.SignalContext -> Signal Action
actions authContext =
  -- use mergeMany if you have other mailboxes or signals to feed into StartApp
  Signal.mergeMany
    [ Signal.map RouterAction TransitRouter.actions
    , Signal.map SetUser ActiveUser.signal
    , Signal.map AuthMsg (Auth.signal authContext)
    ]


mountRoute : Route -> Route -> Model -> (Model, Effects Action)
mountRoute prevRoute route world =
  case route of

    Routes.Home ->
      ( world
      , Topic.Model.fetchAll
        |> Task.toMaybe
        |> Task.map (Maybe.withDefault [])
        |> Task.map TopicsLoad
        |> Effects.task
      )

    Routes.Compose topicId ->
      ( world
      -- since User and Topic live inside Session, we handle a topic update
      -- by updating the Session
      , updateSession <| Session.GoCompose topicId
      )

    Routes.Survey topicId ->
      ( world
      , updateSession <| Session.GoSurvey topicId
      )

    Routes.Read topicId opinionId ->
      ( world
      , updateSession <| Session.GoRead topicId opinionId
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
      TransitRouter.init routerConfig path <| initialModel activeUser

  in
    ( world
    , Effects.batch
      [ fx
      , updateSession <| Session.SetActiveUser activeUser
      ]
    )


initialModel : ActiveUser -> Model
initialModel activeUser =
  { transitRouter = TransitRouter.empty Routes.EmptyRoute
  , topics = []
  , auth = Auth.init activeUser
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

    AuthMsg msg ->
      let
        (update, updateFx) =
          Auth.update
            { next = AuthMsg
            , login = Login
            , logout = (\_ -> Logout)
            }
            (Debug.log "Auth msg" msg)
            world.auth
      in
        ( { world | auth = update }
        , updateFx -- the Login module uses the context to create a World.Action
        )

    Login user ->
      ( world
      , addUser user
      )

    Logout ->
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
      ( world
      , Effects.batch
        [ updateSession <| Session.SetActiveUser (Debug.log "setting active user" activeUser)
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
  Effects.batch
    [ Signal.send ActiveUser.clear ()
        |> Effects.task
    , Signal.send Facebook.address Facebook.Logout
        |> Effects.task
    ]
    |> Effects.map (\_ -> SNoOp "cleared user")


updateSession : Session.Action -> Effects Action
updateSession sessionAction =
  Task.succeed sessionAction
    |> Task.map SessionMsg
    |> Effects.task


view : Signal.Address Action -> Model -> Html
view address world =
  let
    authAddress =
      Signal.forwardTo address AuthMsg

  in
    div []
      [ Auth.viewForm authAddress world.auth
      , Header.view (Auth.viewHeader authAddress world.auth)
      , div
        [ class "world" ]
        ( case TransitRouter.getRoute world of

          Routes.Topics ->
            [ Topic.View.viewAll world.topics ]

          Routes.Home ->
            [ Topic.View.viewAll world.topics ]

          Routes.Survey _ ->
            [ Session.view (Signal.forwardTo address SessionMsg) world.session ]

          Routes.Compose _ ->
            [ Session.view (Signal.forwardTo address SessionMsg) world.session ]

          Routes.Read _ _ ->
            [ Session.view (Signal.forwardTo address SessionMsg) world.session ]

          Routes.EmptyRoute ->
            [ text "" ]
        )
      ]


goHome : Effects Action
goHome =
  Effects.map (\_ -> SNoOp "going home") (Routes.redirect Routes.Topics)
