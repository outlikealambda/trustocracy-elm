module Session
  ( Session
  , init
  , Action
    ( SetUser
    , ClearUser
    , GoCompose
    , GoConnect
    , GoBrowse
    )
  , update
  , view
  ) where


import User exposing (User)
import Topic.Model as Topic exposing (Topic)
import Opinion.Connector as Connector
import Opinion.Composer as Composer
import Opinion.Browser as Browser
import Routes


import Effects exposing (Effects)
import String
import Html exposing (Html, div, h1, text, Attribute)
import Html.Attributes exposing (class)
import Html.Events exposing (on)
import Json.Decode as Json
import TransitRouter



type alias Session =
  { user: User
  , topic: Topic
  , currentView: SessionView
  , composer: Composer.Composer
  , connector: Connector.Connector
  , browser: Browser.Browser
  }


type Action
  -- exposed
  = SetUser User
  | ClearUser
  | GoCompose Int
  | GoConnect Int
  | GoBrowse Int

  -- private
  | TopicMsg Topic.Action
  | PropagateTopic
  | ComposerMsg Composer.Action
  | ConnectorMsg Connector.Action
  | BrowserMsg Browser.Action
  | NoOp


-- the current view
-- we could reuse Routes.Route here, but it feels a little awkward
type SessionView
  = Compose
  | Connect
  | Browse
  | Empty


init : Session
init =
  { user = User.empty
  , topic = Topic.empty
  , currentView = Empty
  , composer = Composer.empty
  , connector = Connector.empty
  , browser = Browser.empty
  }


update : Action -> Session -> (Session, Effects Action)
update action session =
  case action of
    SetUser user ->
      ( { session | user = user }
      , Effects.none
      )

    ClearUser ->
      ( { session | user = User.empty }
      , Effects.none
      )

    -- GoCompose and GoConnect are exposed so that World can still control
    -- routing.
    -- Another approach might be to write an updateFromPath method for each
    -- component, and to pass through a path object whenever the url changes
    GoCompose topicId ->
      let
        (topicUpdate, topicUpdateFx) =
          Topic.init topicId
      in
        ( { session | currentView = Compose }
        , Effects.map TopicMsg topicUpdateFx
        )

    GoConnect topicId ->
      let
        (topicUpdate, topicUpdateFx) =
          Topic.init topicId
      in
        ( { session | currentView = Connect }
        , Effects.map TopicMsg topicUpdateFx
        )

    GoBrowse topicId ->
      let
        (topicUpdate, topicUpdateFx) =
          Topic.init topicId
      in
        ( { session | currentView = Browse }
        , Effects.map TopicMsg topicUpdateFx
        )

    -- PRIVATE
    TopicMsg topicAction ->
      let
        (topicUpdate, topicFx) =
          Topic.update
            { complete = (\_ -> PropagateTopic) }
            topicAction
            session.topic

      in
        ( { session | topic = topicUpdate }
        , topicFx
        )

    PropagateTopic ->
      let
        (composerUpdate, composerUpdateFx) =
          Composer.init session.user session.topic
        (connectorUpdate, connectorUpdateFx) =
          Connector.init session.user session.topic
        (browserUpdate, browserUpdateFx) =
          Browser.init session.topic
      in
        ( { session
          | composer = composerUpdate
          , connector = connectorUpdate
          , browser = browserUpdate
          }
        , Effects.batch
          [ Effects.map ComposerMsg composerUpdateFx
          , Effects.map ConnectorMsg connectorUpdateFx
          , Effects.map BrowserMsg browserUpdateFx
          ]
        )

    ComposerMsg composerAction ->
      let
        (update, updateFx) =
          Composer.update composerAction session.composer
      in
        ( { session | composer = update }
          , Effects.map ComposerMsg updateFx
        )

    ConnectorMsg connectAction ->
      let
        (update, updateFx) =
          Connector.update connectAction session.connector
      in
        ( { session | connector = update }
        , Effects.map ConnectorMsg updateFx
        )

    BrowserMsg browserAction ->
      let
        (update, updateFx) =
          Browser.update browserAction session.browser
      in
        ( { session | browser = update }
        , Effects.map BrowserMsg updateFx
        )


    NoOp ->
      ( session, Effects.none )


view : Signal.Address Action -> Session -> Html
view address session =
  div
    [ class "session" ]
    [ sessionHeader session
    , sessionContent address session
    ]


-- used to help create the nav links
type alias SessionLinker a =
  { routeView : SessionView
  , buildRoute : Int -> Routes.Route
  , makeHtml : a -> Html
  , getter : Session -> a
  }


-- builds a link, setting it to active if it matches the current view
buildLink : SessionLinker a -> Session -> Html
buildLink {routeView, buildRoute, makeHtml, getter} session =
  let
    classes =
      -- we need to examine the current view to determine whether this
      -- link is active or not
      -- this is the primary reason for this class; it is awkward to write 3
      -- if statements in sessionHeader
      if routeView == session.currentView then
        [ "session-link", "active"]
      else
        [ "session-link" ]
  in
    div
      [ class <| String.join " " classes
      , clickToDiv <| buildRoute session.topic.id ]
      [ makeHtml <| getter session
      ]


composeLinker : Session -> Html
composeLinker = buildLink
  { routeView = Compose
  , buildRoute = Routes.Compose
  , makeHtml = Composer.navButton
  , getter = .composer
  }


connectLinker : Session -> Html
connectLinker = buildLink
  { routeView = Connect
  , buildRoute = Routes.Connect
  , makeHtml = Connector.navButton
  , getter = .connector
  }


browseLinker : Session -> Html
browseLinker = buildLink
  { routeView = Browse
  , buildRoute = Routes.Browse
  , makeHtml = Browser.navButton
  , getter = .browser
  }


sessionHeader : Session -> Html
sessionHeader session =
  div
    [ class "session-overview" ]
    [ h1 [ class "topic-title" ] [ text session.topic.text ]
    , div
      [ class "session-links" ]
      [ browseLinker session
      , connectLinker session
      , composeLinker session
      ]
    ]


sessionContent : Signal.Address Action -> Session -> Html
sessionContent address session =
  case session.currentView of
    Empty ->
      div [] [ text "whoops, why we here?" ]
    Connect ->
      div
        [ class "content" ]
        (Connector.view (Signal.forwardTo address ConnectorMsg) session.connector)
    Compose ->
      Composer.view (Signal.forwardTo address ComposerMsg) session.composer
    Browse ->
      div
        [ class "content" ]
        [ Browser.view session.browser ]


clickToDiv : Routes.Route -> Attribute
clickToDiv route =
  let
    path =
      Routes.encode route
  in
    on
      "click"
      Json.value
      (\_ -> Signal.message TransitRouter.pushPathAddress path)
