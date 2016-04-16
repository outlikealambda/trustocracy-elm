module Session
  ( Session
  , init
  , Action
    ( SetActiveUser
    , GoCompose
    , GoSurvey
    , GoRead
    , GoUserDelegates
    )
  , update
  , view
  ) where


import ActiveUser exposing
  ( ActiveUser
    ( LoggedIn
    , LoggedOut
    )
  )
import Topic.Model as Topic exposing (Topic)
import Opinion.Surveyor as Surveyor exposing (Surveyor)
import Opinion.Composer as Composer exposing (Composer)
import Delegator exposing (Delegator)
import Routes


import Effects exposing (Effects)
import String
import Task
import Html exposing
  ( Html
  , div
  , h1
  , text
  )
import Html.Attributes exposing (class)



type alias Session =
  { activeUser : ActiveUser
  , topic : Topic
  , currentView : SessionView
  , composer : Composer
  , surveyor : Surveyor
  , delegator : Delegator
  }

type alias TopicId = Int
type alias OpinionId = Int


type Action
  -- exposed
  = SetActiveUser ActiveUser
  | GoCompose TopicId
  | GoSurvey TopicId
  | GoRead TopicId OpinionId
  | GoUserDelegates

  -- private
  | TopicMsg Topic.Action
  | PropagateTopic
  | ComposerMsg Composer.Action
  | SurveyorMsg Surveyor.Action
  | DelegatorMsg Delegator.Action
  | NoOp


-- the current view
-- we could reuse Routes.Route here, but it feels a little awkward
type SessionView
  = Compose
  | Survey
  | UserDelegates
  | Empty


init : Session
init =
  { activeUser = LoggedOut
  , topic = Topic.empty
  , currentView = Empty
  , composer = Composer.empty
  , surveyor = Surveyor.empty
  , delegator = Delegator.empty
  }


update : Action -> Session -> (Session, Effects Action)
update action session =
  case action of
    SetActiveUser activeUser ->
      ( { session | activeUser = activeUser }
      , Effects.none
      )

    -- GoCompose and GoSurvey are exposed so that World can still control
    -- routing.
    -- Another approach might be to write an updateFromPath method for each
    -- component, and to pass through a path object whenever the url changes
    GoCompose topicId ->
      setSessionTopic session Compose topicId

    GoSurvey topicId ->
      let
        (sessionUpdate, sessionUpdateFx) =
          setSessionTopic session Survey topicId
      in
        ( { sessionUpdate | surveyor = Surveyor.blur sessionUpdate.surveyor }
        , sessionUpdateFx )

    -- we'll focus the Surveyor instead of using a separate reader
    GoRead topicId opinionId ->
      let
        (sessionUpdate, sessionUpdateFx) =
          setSessionTopic session Survey topicId
      in
        ( { sessionUpdate | surveyor = Surveyor.focus opinionId sessionUpdate.surveyor }
        , sessionUpdateFx )


    GoUserDelegates ->
      ( { session | currentView = UserDelegates }, Effects.none )

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

    -- we only propagate topic, and not user, because
    -- the app shouldn't normally be switching users, and only
    -- does so in the current dev environment
    PropagateTopic ->
      updateViews session

    ComposerMsg composerAction ->
      let
        (update, updateFx) =
          Composer.update composerAction session.composer
      in
        ( { session | composer = update }
          , Effects.map ComposerMsg updateFx
        )

    SurveyorMsg connectAction ->
      let
        (update, updateFx) =
          Surveyor.update connectAction session.surveyor
      in
        ( { session | surveyor = update }
        , Effects.map SurveyorMsg updateFx
        )

    DelegatorMsg userDelegatesAction ->
      let
        (update, updateFx) =
          Delegator.update userDelegatesAction session.delegator
      in
        ( { session | delegator = update }
        , Effects.map DelegatorMsg updateFx
        )

    NoOp ->
      ( session, Effects.none )


setSessionTopic : Session -> SessionView -> TopicId -> (Session, Effects Action)
setSessionTopic session newSessionView topicId =
  if session.topic.id == topicId then
    -- same topic, change the view, but no need to reload
    -- the models
    ( { session | currentView = newSessionView }
    , Effects.none )
  else

    let
      (topicInit, topicInitFx) =
        Topic.init topicId

    in
      ( { session
        | currentView = newSessionView
        , topic = topicInit
        }
      , Effects.map TopicMsg topicInitFx
      )


-- if the user is LoggedOut, we don't need to update
-- compose and survey
updateViews : Session -> (Session, Effects Action)
updateViews session =
  case session.activeUser of
    LoggedOut ->
        ( session
        , Task.succeed (Surveyor.Init session.topic session.activeUser)
          |> Effects.task
          |> Effects.map SurveyorMsg
        )
    LoggedIn user ->
      let
        (composerUpdate, composerUpdateFx) =
          Composer.init user session.topic
      in
        ( { session
          | composer = composerUpdate
          }
        , Effects.batch
          [ Effects.map ComposerMsg composerUpdateFx
          , Task.succeed (Surveyor.Init session.topic session.activeUser)
            |> Effects.task
            |> Effects.map SurveyorMsg
          ]
        )


view : Signal.Address Action -> Session -> Html
view address session =
  let
    sessionContent =
      case session.activeUser of
        LoggedIn user ->
          activeSessionContent address session

        LoggedOut ->
          inactiveSessionContent address session
  in
    div [ class "session" ] sessionContent


-- used to help create the nav links
type alias SessionLinker a =
  { routeView : SessionView
  , buildRoute : Int -> Routes.Route
  , makeHtml : a -> Html
  , getter : Session -> a
  }


-- builds a link, setting it to active if it matches the current view
buildNavLink : SessionLinker a -> Session -> Html
buildNavLink {routeView, buildRoute, makeHtml, getter} session =
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
      , Routes.goToRoute <| buildRoute session.topic.id ]
      [ makeHtml <| getter session
      ]


composeLinker : Session -> Html
composeLinker = buildNavLink
  { routeView = Compose
  , buildRoute = Routes.Compose
  , makeHtml = Composer.navButton
  , getter = .composer
  }


connectLinker : Session -> Html
connectLinker = buildNavLink
  { routeView = Survey
  , buildRoute = Routes.Survey
  , makeHtml = Surveyor.navButton
  , getter = .surveyor
  }


activeSessionHeader : Session -> Html
activeSessionHeader session =
  div
    [ class "session-overview" ]
    [ h1 [ class "topic-title" ] [ text session.topic.text ]
    , div
      [ class "session-links" ]
      [ connectLinker session
      , composeLinker session
      ]
    ]


inactiveSessionHeader : Session -> Html
inactiveSessionHeader session =
  div
    [ class "session-overview" ]
    [ h1 [ class "topic-title" ] [ text session.topic.text ]
    , div
      [ class "session-links" ]
      [ connectLinker session
      ]
    ]


activeSessionContent : Signal.Address Action -> Session -> List Html
activeSessionContent address session =
  case session.currentView of

    Survey ->
      [ activeSessionHeader session
      , div
        [ class "content" ]
        <| Surveyor.view
          { address = Signal.forwardTo address SurveyorMsg
          , routeBuilder = Routes.Read session.topic.id
          }
          session.surveyor
      ]

    Compose ->
      [ activeSessionHeader session
      , div
        [ class "content" ]
        [ Composer.view (Signal.forwardTo address ComposerMsg) session.composer ]
      ]

    UserDelegates ->
      [ div
        [ class "content" ]
        [ Delegator.view (Signal.forwardTo address DelegatorMsg) session.delegator ]
      ]

    Empty ->
      [ div [] [ text "whoops, why we here?" ] ]


-- an inactiveSession should only route to browse
inactiveSessionContent : Signal.Address Action -> Session -> List Html
inactiveSessionContent address session =
  [ inactiveSessionHeader session
  , case session.currentView of
      Survey ->
        div
          [ class "content" ]
          <| Surveyor.view
            { address = Signal.forwardTo address SurveyorMsg
            , routeBuilder = Routes.Read session.topic.id
            }
            session.surveyor

      _ ->
        div
          []
          [ text "Sorry, you must be logged in to see this content" ]
  ]
