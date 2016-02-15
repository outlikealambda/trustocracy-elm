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


import Html exposing (Html, div, h2, text)
import Html.Attributes exposing (class)
import Effects exposing (Effects)
import Task


type alias Session =
  { user: User
  , topic: Topic
  , currentView: CurrentView
  , composer: Composer.Model
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
          Topic.update topicAction session.topic

        -- If the state of the topic.isComplete has changed
        -- from False -> True, then we need to update the child components.
        --
        -- When we load a connector/composer view, we first need to load the
        -- Topic, then wait for the Effects to finish (since the effect is what
        -- goes to the DB and retrieves the Topic).  Once the Topic is
        -- retrieved we can then go ahead and load the composer/connector.
        --
        -- Maybe there's a better way to do this, but chaining effects is
        -- difficult (impossible?), so we can't do Topic.update `andThen` ...
        propagationFx =
          if Topic.justCompleted session.topic topicUpdate then
            Task.succeed PropagateTopic |> Effects.task
          else
            Effects.none

      in
        ( { session | topic = topicUpdate }
        , Effects.batch
          [ Effects.map TopicMsg topicFx
          , propagationFx
          ]
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
  let
    header =
      h2 [ class "topic-title" ] [ text session.topic.text ]
    content =
      case session.currentView of
        Empty ->
          div [] [ text "whoops, why we here?" ]
        Connect ->
          div
            [ class "row" ]
            (Connector.view (Signal.forwardTo address ConnectorMsg) session.connector)
        Compose ->
          Composer.view (Signal.forwardTo address ComposerMsg) session.composer
  in
    div
      [ class "session" ]
      [ header
      , content
      ]
