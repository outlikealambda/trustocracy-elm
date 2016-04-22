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


import Auth exposing (Auth)
import Common.API as API
import Header
import Session exposing (Session)
import Topic.Model exposing (Topic)
import Topic.View


import Routes exposing (Route)
import TransitRouter


type alias Model = TransitRouter.WithRoute Routes.Route
  { session : Session
  , topics : List Topic
  }


type Action
  = SessionMsg Session.Action
  | TopicsLoad (List Topic)
  | RouterAction (TransitRouter.Action Routes.Route)
  | SNoOp String


actions : Auth.SignalContext -> Signal Action
actions authContext =
  -- use mergeMany if you have other mailboxes or signals to feed into StartApp
  Signal.mergeMany
    [ Signal.map RouterAction TransitRouter.actions
    , Signal.map SessionMsg (Session.signal authContext)
    ]


mountRoute : Route -> Route -> Model -> (Model, Effects Action)
mountRoute prevRoute route world =
  case route of

    Routes.Home ->
      ( world
      , API.fetchAllTopics (TopicsLoad << Maybe.withDefault [])
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
      , API.fetchAllTopics (TopicsLoad << Maybe.withDefault [])
      )

    Routes.UserDelegates ->
      ( world
      , updateSession <| Session.GoUserDelegates
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


init : String -> (Model, Effects Action)
init path =
  let
    (initialWorld, initialWorldFx) =
      initialModel
    (world, fx) =
      TransitRouter.init routerConfig path initialWorld
  in
    ( world
    , Effects.batch
      [ fx
      , initialWorldFx
      ]
    )


initialModel : (Model, Effects Action)
initialModel =
  let
    (session, sessionFx) =
      Session.init
  in
    ( { transitRouter = TransitRouter.empty Routes.EmptyRoute
      , topics = []
      , session = session
      }
    , Effects.map SessionMsg sessionFx )


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


updateSession : Session.Action -> Effects Action
updateSession sessionAction =
  Task.succeed sessionAction
    |> Task.map SessionMsg
    |> Effects.task


view : Signal.Address Action -> Model -> Html
view address world =
  div []
    [ Header.view
      <| Session.navHeader (Signal.forwardTo address SessionMsg) world.session
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

        Routes.UserDelegates ->
          [ Session.view (Signal.forwardTo address SessionMsg) world.session ]

        Routes.EmptyRoute ->
          [ text "" ]
      )
    ]


goHome : Effects Action
goHome =
  Effects.map (\_ -> SNoOp "going home") (Routes.redirect Routes.Topics)
