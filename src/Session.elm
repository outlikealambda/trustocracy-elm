module Session
  ( Session
  , init
  , Action
    ( SetUser
    , ClearUser
    , GoCompose
    , GoConnect
    )
  , update
  , view
  ) where


import User exposing (User)
import Topic.Model as Topic exposing (Topic)
import Opinion.Connector as Connector
import Opinion.Composer as Composer
import Routes


import Effects exposing (Effects)
import Html exposing (Html, div, a, h1, text, Attribute)
import Html.Attributes exposing (class, href)
import Html.Events exposing (onWithOptions)
import Json.Decode as Json
import TransitRouter



type alias Session =
  { user: User
  , topic: Topic
  , currentView: CurrentView
  , composer: Composer.Composer
  , connector: Connector.Model
  }


type Action
  -- exposed
  = SetUser User
  | ClearUser
  | GoCompose Int
  | GoConnect Int

  -- private
  | TopicMsg Topic.Action
  | PropagateTopic
  | ComposerMsg Composer.Action
  | ConnectorMsg Connector.Action
  | NoOp


type CurrentView
  = Compose
  | Connect
  | Empty


init : Session
init =
  { user = User.empty
  , topic = Topic.empty
  , currentView = Empty
  , composer = Composer.empty
  , connector = Connector.empty
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
      in
        ( { session
          | composer = composerUpdate
          , connector = connectorUpdate
          }
        , Effects.batch
          [ Effects.map ComposerMsg composerUpdateFx
          , Effects.map ConnectorMsg connectorUpdateFx
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
      let (update, updateFx) =
          Connector.update connectAction session.connector
      in
        ( { session | connector = update }
        , Effects.map ConnectorMsg updateFx
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

sessionHeader : Session -> Html
sessionHeader session =
  let
    link =
      case session.currentView of
        Connect ->
          a
            (clickTo <| Routes.Compose session.topic.id )
            [ text <| "Compose" ]
        Compose ->
          a
            (clickTo <| Routes.Connect session.topic.id )
            [ text <| "Connect" ]
        Empty ->
          div [] []
  in
    div
      [ class "session-overview" ]
      [ h1 [ class "topic-title" ] [ text session.topic.text ]
      , link
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


clickTo : Routes.Route -> List Attribute
clickTo route =
  let
    path =
      Routes.encode route
  in
    [ href path
    , onWithOptions
      "click"
      { stopPropagation = True, preventDefault = True }
      Json.value
      (\_ -> Signal.message TransitRouter.pushPathAddress path)
    ]
