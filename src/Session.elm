module Session
  ( Session
  , signal
  , init
  , Action
    ( GoCompose
    , GoSurvey
    , GoRead
    , GoUserDelegates
    )
  , update
  , view
  , navHeader
  ) where


import ActiveUser exposing
  ( ActiveUser
    ( LoggedIn
    , LoggedOut
    )
  )
import Auth exposing (Auth)
import Common.API as API
import Delegator exposing (Delegator)
import Opinion.Surveyor as Surveyor exposing (Surveyor)
import Opinion.Composer as Composer exposing (Composer)
import Routes
import Topic.Model as Topic exposing (Topic)
import User exposing (User)


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
  , auth : Auth
  , delegator : Delegator
  }

type alias TopicId = Int
type alias OpinionId = Int


type Action
  -- exposed
  = GoCompose TopicId
  | GoSurvey TopicId
  | GoRead TopicId OpinionId
  | GoUserDelegates

  -- private
  | SetTopic (Maybe Topic)

  -- exposed to children
  | SetActiveUser ActiveUser

  -- child modules
  | ComposerMsg Composer.Action
  | SurveyorMsg Surveyor.Action
  | DelegatorMsg Delegator.Action
  | AuthMsg Auth.Action


-- the current view
-- we could reuse Routes.Route here, but it feels a little awkward
type SessionView
  = Compose
  | Survey
  | UserDelegates
  | Empty


signal : Auth.SignalContext -> Signal Action
signal authContext =
  Signal.map AuthMsg (Auth.signal authContext)


init : (Session, Effects Action)
init =
  let
    (auth, authFx) =
      Auth.init
  in
    ( { activeUser = LoggedOut
      , topic = Topic.empty
      , currentView = Empty
      , composer = Composer.empty
      , surveyor = Surveyor.empty
      , auth = auth
      , delegator = Delegator.fromActiveUser LoggedOut
      }
    , Effects.map AuthMsg authFx
    )


update : Action -> Session -> (Session, Effects Action)
update action session =
  case action of
    -- TODO: handle ActiveUser.LoggedOut
    SetActiveUser activeUser ->
      ( { session
        | activeUser = activeUser
        , delegator = Delegator.fromActiveUser activeUser
      }
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

    -- we only propagate topic, and not user, because
    -- the app shouldn't normally be switching users, and only
    -- does so in the current dev environment?
    -- TODO: reload views on user change/logout
    SetTopic maybeTopic ->
      case maybeTopic of
        Nothing ->
          ( session, Effects.none )
        Just topic ->
          updateViews { session | topic = topic }

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

    AuthMsg authAction ->
      let
        (update, updateFx) =
          Auth.update
            { next = AuthMsg
            , setUser = SetActiveUser
            }
            authAction
            session.auth
      in
        ( { session | auth = update }
        , updateFx )

    DelegatorMsg delegateAction ->
      case session.activeUser of

        LoggedOut ->
          ( session, Effects.none )

        LoggedIn user ->
          let
            (update, updateFx) =
              Delegator.update delegateAction session.delegator
          in
            ( { session
              | delegator = update
              , activeUser = LoggedIn { user | trustees = update.saved }
            }
            , Effects.map DelegatorMsg updateFx )


setSessionTopic : Session -> SessionView -> TopicId -> (Session, Effects Action)
setSessionTopic session newSessionView topicId =
  let
    fx =
      if session.topic.id == topicId then
        Effects.none
      else
        API.fetchTopic SetTopic topicId
  in
    ( { session | currentView = newSessionView }
    , fx
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
          activeSessionContent address user session

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
buildSubNavLink : SessionLinker a -> Session -> Html
buildSubNavLink {routeView, buildRoute, makeHtml, getter} session =
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
composeLinker = buildSubNavLink
  { routeView = Compose
  , buildRoute = Routes.Compose
  , makeHtml = Composer.navButton
  , getter = .composer
  }


connectLinker : Session -> Html
connectLinker = buildSubNavLink
  { routeView = Survey
  , buildRoute = Routes.Survey
  , makeHtml = Surveyor.navButton
  , getter = .surveyor
  }


activeSubNav : Session -> Html
activeSubNav session =
  div
    [ class "session-overview" ]
    [ h1 [ class "topic-title" ] [ text session.topic.text ]
    , div
      [ class "session-links" ]
      [ connectLinker session
      , composeLinker session
      ]
    ]


inactiveSubNav : Session -> Html
inactiveSubNav session =
  div
    [ class "session-overview" ]
    [ h1 [ class "topic-title" ] [ text session.topic.text ]
    , div
      [ class "session-links" ]
      [ connectLinker session
      ]
    ]


activeSessionContent : Signal.Address Action -> User -> Session -> List Html
activeSessionContent address user session =
  case session.currentView of

    Survey ->
      [ activeSubNav session
      , div
        [ class "content" ]
        [ Surveyor.view
          { address = Signal.forwardTo address SurveyorMsg
          , readRouteBuilder = Routes.Read session.topic.id
          , showAllRoute = Routes.Survey session.topic.id
          }
          session.surveyor
        ]
      ]

    Compose ->
      [ activeSubNav session
      , div
        [ class "content" ]
        [ Composer.view (Signal.forwardTo address ComposerMsg) session.composer ]
      ]

    UserDelegates ->
      [ div
        [ class "content" ]
        [ Delegator.view (Signal.forwardTo address DelegatorMsg) user.emails session.delegator ]
      ]

    Empty ->
      [ div [] [ text "whoops, why we here?" ] ]


-- an inactiveSession should only route to browse
inactiveSessionContent : Signal.Address Action -> Session -> List Html
inactiveSessionContent address session =
  case session.currentView of
    Survey ->
      [ inactiveSubNav session
      , div
        [ class "content" ]
        [ Surveyor.view
          { address = Signal.forwardTo address SurveyorMsg
          , readRouteBuilder = Routes.Read session.topic.id
          , showAllRoute = Routes.Survey session.topic.id
          }
          session.surveyor
        ]
      ]

    _ ->
      [ inactiveSubNav session
      , div
        [ class "content" ]
        [ text "Sorry, you must be logged in to see this content" ]
      ]


navHeader : Signal.Address Action -> Session -> List Html
navHeader address {auth, activeUser} =
    Auth.view
      { address = Signal.forwardTo address AuthMsg
      , activeUser = activeUser
      }
      auth
    ++ Delegator.navHeader activeUser
