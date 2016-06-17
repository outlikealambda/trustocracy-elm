module World exposing
  ( Msg
  , World
  , init
  , view
  , subscriptions
  , update
  )


import Html exposing (Html, input, div, node, h1, text)
import Html.Attributes exposing (class, rel, href, placeholder, value, style)
import Utils.Cmd as CmdUtils

import Common.API as API
import Header
import Location
import Model.Topic as Topic exposing (Topic)
import Routes exposing (Route)
import Session exposing (Session)
import Topic.View


type alias World =
  { route : Route
  , session : Session
  , topics : List Topic
  , currentView : WorldView
  }


type WorldView = SessionView | TopicsView

type Msg
  = GoSession Session.Msg -- temp hack to let TopicView change the model state
  | SessionMsg Session.Msg
  | TopicsLoadComplete (List Topic)
  | TopicsLoadFailed (String)
  | SetPath String
  | PathUpdated String
  | SNoOp String


subscriptions : World -> Sub Msg
subscriptions _ =
  Sub.batch
    [ Location.pathUpdates PathUpdated
    , Sub.map SessionMsg Session.subscriptions
    ]


mountRoute : Route -> World -> (World, Cmd Msg)
mountRoute route world =
  let
    fx =
      case (Debug.log "mounting route" route) of
        Routes.Home ->
          API.fetchAllTopics TopicsLoadFailed TopicsLoadComplete

        Routes.Compose topicId ->
          -- since User and Topic live inside Session, we handle a topic update
          -- by updating the Session
          updateSession <| Session.GoCompose topicId

        Routes.Explore topicId ->
          updateSession <| Session.GoExplore topicId

        -- Routes.Survey topicId ->
        --   updateSession <| Session.GoSurvey topicId

        Routes.Read topicId opinionId ->
          updateSession <| Session.GoRead topicId opinionId

        -- map to [] if fail, since this will probably be the
        -- home page and we don't want to continually redirect
        Routes.Topics ->
          API.fetchAllTopics TopicsLoadFailed TopicsLoadComplete

        Routes.UserDelegates ->
          updateSession <| Session.GoUserDelegates

        Routes.EmptyRoute ->
          Cmd.none
  in
    ( { world | route = route }, fx )


init : String -> (World, Cmd Msg)
init path =
  let
    (initialWorld, initialWorldFx) =
      initialModel
    (world, fx) =
      mountRoute (Routes.decode (Debug.log "initial path" path)) initialWorld
  in
    ( world
    , Cmd.batch
      [ fx
      , initialWorldFx
      ]
    )


initialModel : (World, Cmd Msg)
initialModel =
  let
    (session, sessionFx) =
      Session.init
  in
    ( { topics = []
      , session = session
      , route = Routes.EmptyRoute
      , currentView = TopicsView
      }
    , Cmd.map SessionMsg sessionFx )


update : Msg -> World -> (World, Cmd Msg)
update message world =
  case message of
    GoSession sessionAction ->
      ( { world | currentView = SessionView }
      , CmdUtils.init <| SessionMsg sessionAction
      )

    SessionMsg sessionAction ->
      let
        (update, updateFx) =
          Session.update (Debug.log "session action" sessionAction) world.session
      in
        ( { world | session = update }
        , Cmd.map SessionMsg updateFx
        )

    SetPath path ->
      ( world, Location.setPath (Debug.log "SetPath" path) )

    PathUpdated path ->
      mountRoute (Routes.decode (Debug.log "PathUpdated" path)) world

    TopicsLoadComplete topics ->
      ( { world | topics = (Debug.log "topics load complete" topics) }
      , Cmd.none )

    TopicsLoadFailed err ->
      let
        msg = Debug.log "Failed to load topics" err
      in
        ( { world | topics = [] }
        , Cmd.none )

    SNoOp str ->
      let
        _ = Debug.log "SNoOp" str
      in
        ( world
        , Cmd.none
        )


updateSession : Session.Msg -> Cmd Msg
updateSession sessionAction =
  CmdUtils.init (SessionMsg sessionAction)


view : World -> Html Msg
view world =
  div []
    [ Header.view
      (SetPath << Routes.encode)
      <| Session.navHeader SessionMsg world.session
    , div
      [ class "world" ]
      ( case (Debug.log "world view " world.currentView) of
        TopicsView ->
          [ Topic.View.viewAll (GoSession << Session.GoExplore) world.topics ]

        SessionView ->
          [ Session.view SessionMsg world.session ]

        -- Routes.Topics ->
        --   -- [ Topic.View.viewAll (SetPath << Routes.encode) world.topics ]
        --   [ Topic.View.viewAll (SessionMsg << Session.GoExplore) world.topics ]
        --
        -- Routes.Home ->
        --   -- [ Topic.View.viewAll (SetPath << Routes.encode) world.topics ]
        --   [ Topic.View.viewAll (SessionMsg << Session.GoExplore) world.topics ]
        --
        -- -- Routes.Survey _ ->
        -- --   [ Session.view SessionMsg world.session ]
        --
        -- Routes.Compose _ ->
        --   [ Session.view SessionMsg world.session ]
        --
        -- Routes.Explore _ ->
        --   [ Session.view SessionMsg world.session ]
        --
        -- Routes.Read _ _ ->
        --   [ Session.view SessionMsg world.session ]
        --
        -- Routes.UserDelegates ->
        --   [ Session.view SessionMsg world.session ]
        --
        -- Routes.EmptyRoute ->
        --   [ text "" ]
      )
    ]
