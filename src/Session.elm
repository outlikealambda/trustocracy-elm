module Session exposing
  ( Session
  , subscriptions
  , init
  , Msg
    ( GoCompose
    , GoSurvey
    , GoRead
    , GoUserDelegates
    )
  , update
  , view
  , navHeader
  )


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
import Location
import Routes
import Topic.Model as Topic exposing (Topic)
import User exposing (User)

import Html exposing
  ( Html
  , div
  , h1
  , text
  )
import Html.App
import Html.Attributes exposing (class)
import Platform.Cmd exposing (Cmd)
import String


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


type Msg
  -- exposed
  = GoCompose TopicId
  | GoSurvey TopicId
  | GoRead TopicId OpinionId
  | GoUserDelegates

  -- private
  | Error String
  | SetTopic Topic
  | SetPath Routes.Route

  -- exposed to children
  | SetActiveUser ActiveUser

  -- child modules
  | ComposerMsg Composer.Msg
  | SurveyorMsg Surveyor.Msg
  | DelegatorMsg Delegator.Msg
  | AuthMsg Auth.Msg


-- the current view
-- we could reuse Routes.Route here, but it feels a little awkward
type SessionView
  = Compose
  | Survey
  | UserDelegates
  | Empty


subscriptions : Sub Msg
subscriptions =
  Sub.map AuthMsg Auth.subscriptions


init : (Session, Cmd Msg)
init =
  let
    (auth, authFx) =
      Auth.init
  in
    { activeUser = LoggedOut
    , topic = Topic.empty
    , currentView = Empty
    , composer = Composer.empty
    , surveyor = Surveyor.empty
    , auth = auth
    , delegator = Delegator.fromActiveUser LoggedOut
    }
    ! [ Cmd.map AuthMsg authFx ]


update : Msg -> Session -> (Session, Cmd Msg)
update action session =
  case action of
    -- TODO: handle ActiveUser.LoggedOut
    SetActiveUser activeUser ->
      { session
      | activeUser = activeUser
      , delegator = Delegator.fromActiveUser activeUser
      }
      ! []

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
        { sessionUpdate | surveyor = Surveyor.blur sessionUpdate.surveyor }
        ! [ sessionUpdateFx ]

    -- we'll focus the Surveyor instead of using a separate reader
    GoRead topicId opinionId ->
      let
        (sessionUpdate, sessionUpdateFx) =
          setSessionTopic session Survey topicId
      in
        { sessionUpdate | surveyor = Surveyor.focus opinionId sessionUpdate.surveyor }
        ! [ sessionUpdateFx ]

    GoUserDelegates ->
      { session | currentView = UserDelegates } ! []

    -- PRIVATE
    Error err ->
      let
        msg = Debug.log "error!" err
      in
        session ! []

    -- we only propagate topic, and not user, because
    -- the app shouldn't normally be switching users, and only
    -- does so in the current dev environment?
    -- TODO: reload views on user change/logout
    SetTopic topic ->
      updateViews { session | topic = topic }

    SetPath route ->
      session ! [ Location.setPath <| Routes.encode route ]

    ComposerMsg composerAction ->
      let
        (update, updateFx) =
          Composer.update composerAction session.composer
      in
        { session
        | composer = update
        }
        ! [ Cmd.map ComposerMsg updateFx ]

    SurveyorMsg connectAction ->
      let
        (update, updateFx) =
          Surveyor.update
            session.topic
            session.activeUser
            connectAction
            session.surveyor
      in
        { session
        | surveyor = update
        }
        ! [ Cmd.map SurveyorMsg updateFx ]

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
        { session
        | auth = update
        }
        ! [ updateFx ]

    DelegatorMsg delegateAction ->
      case session.activeUser of

        LoggedOut ->
          session ! []

        LoggedIn user ->
          let
            (update, updateFx) =
              Delegator.update delegateAction session.delegator

            nextSession =
              { session
              | delegator = update
              , activeUser =
                  LoggedIn
                    { user
                    | trustees = update.saved
                    }
              }
          in
            nextSession ! [ Cmd.map DelegatorMsg updateFx ]


setSessionTopic : Session -> SessionView -> TopicId -> (Session, Cmd Msg)
setSessionTopic session newSessionView topicId =
  let
    cmds =
      if session.topic.id == topicId then
        []
      else
        [ API.fetchTopic Error SetTopic topicId ]
  in
    { session
    | currentView = newSessionView
    }
    ! cmds

-- if the user is LoggedOut, we don't need to update
-- compose and survey
updateViews : Session -> (Session, Cmd Msg)
updateViews session =
  let
    (surveyor, surveyorFx) =
      Surveyor.init session.topic session.activeUser
  in
    case session.activeUser of
      LoggedOut ->
          { session
          | surveyor = surveyor
          }
          ! [ Cmd.map SurveyorMsg surveyorFx ]

      LoggedIn user ->
        let
          (composer, composerFx) =
            Composer.init user session.topic
        in
          { session
          | composer = composer
          , surveyor = surveyor
          }
          ! [ Cmd.map ComposerMsg composerFx
            , Cmd.map SurveyorMsg surveyorFx
            ]


view : (Msg -> msg) -> Session -> Html msg
view transform session =
  let
    sessionContent =
      case session.activeUser of
        LoggedIn user ->
          activeSessionContent user session

        LoggedOut ->
          inactiveSessionContent session
  in
    Html.App.map transform <| div [ class "session" ] sessionContent


-- used to help create the nav links
type alias SessionLinker a =
  { routeView : SessionView
  , buildRoute : Int -> Routes.Route
  , makeHtml : a -> Html Msg
  , getter : Session -> a
  }


-- builds a link, setting it to active if it matches the current view
buildSubNavLink : SessionLinker a -> Session -> Html Msg
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
      , Routes.goToRoute <| SetPath <| buildRoute session.topic.id ]
      [ makeHtml <| getter session
      ]


composeLinker : Session -> Html Msg
composeLinker = buildSubNavLink
  { routeView = Compose
  , buildRoute = Routes.Compose
  , makeHtml = Composer.navButton
  , getter = .composer
  }


connectLinker : Session -> Html Msg
connectLinker = buildSubNavLink
  { routeView = Survey
  , buildRoute = Routes.Survey
  , makeHtml = Surveyor.navButton
  , getter = .surveyor
  }


activeSubNav : Session -> Html Msg
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


inactiveSubNav : Session -> Html Msg
inactiveSubNav session =
  div
    [ class "session-overview" ]
    [ h1 [ class "topic-title" ] [ text session.topic.text ]
    , div
      [ class "session-links" ]
      [ connectLinker session
      ]
    ]


activeSessionContent : User -> Session -> List (Html Msg)
activeSessionContent user session =
  case session.currentView of

    Survey ->
      [ activeSubNav session
      , div
        [ class "content" ]
        [ Surveyor.view
          { transform = SurveyorMsg
          , readRouteBuilder = Routes.Read session.topic.id
          , showAllRoute = Routes.Survey session.topic.id
          , topic = session.topic
          , activeUser = session.activeUser
          }
          session.surveyor
        ]
      ]

    Compose ->
      [ activeSubNav session
      , div
        [ class "content" ]
        [ Html.App.map ComposerMsg (Composer.view session.composer) ]
      ]

    UserDelegates ->
      [ div
        [ class "content" ]
        [ Html.App.map DelegatorMsg (Delegator.view user.emails session.delegator) ]
      ]

    Empty ->
      [ div [] [ text "whoops, why we here?" ] ]


-- an inactiveSession should only route to browse
inactiveSessionContent : Session -> List (Html Msg)
inactiveSessionContent session =
  case session.currentView of
    Survey ->
      [ inactiveSubNav session
      , div
        [ class "content" ]
        [ Surveyor.view
          { transform = SurveyorMsg
          , readRouteBuilder = Routes.Read session.topic.id
          , showAllRoute = Routes.Survey session.topic.id
          , topic = session.topic
          , activeUser = session.activeUser
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


navHeader : (Msg -> msg) -> Session -> List (Html msg)
navHeader transform {auth, activeUser} =
  List.append
    (Auth.view
      { transform = (transform << AuthMsg)
      , activeUser = activeUser
      }
      auth)
    (Delegator.navHeader (transform << DelegatorMsg) activeUser)
