module Session exposing
  ( Session
  , subscriptions
  , init
  , Msg
    ( GoCompose
    , GoExplore
    , GoConnect
    , GoBrowse
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

import Model.Explorer as Explorer exposing (Explorer)
import Model.Topic as Topic exposing (Topic)
import Model.User exposing (User)
import Update.Explorer as ExplorerUpdate
import View.Explorer as ExplorerView

import Opinion.Composer as Composer exposing (Composer)

import Html exposing (Html)
import Html.App
import Html.Attributes as Attributes exposing (class)
import Html.Events as Events
import Platform.Cmd exposing (Cmd)


type alias Session =
  { activeUser : ActiveUser
  , topic : Topic
  , currentView : SessionView
  , composer : Composer
  , connector : Explorer
  , browser : Explorer
  , auth : Auth
  , delegator : Delegator
  }


type alias TopicId = Int
type alias OpinionId = Int


type Msg
  -- exposed
  = GoCompose TopicId
  | GoExplore TopicId
  | GoBrowse TopicId
  | GoConnect TopicId
  | GoUserDelegates

  -- private
  | Error String
  | SetTopic Topic

  -- exposed to children
  | SetActiveUser ActiveUser

  -- child modules
  | ComposerMsg Composer.Msg
  | DelegatorMsg Delegator.Msg
  | ConnectorMsg ExplorerUpdate.Msg
  | BrowserMsg ExplorerUpdate.Msg
  | AuthMsg Auth.Msg


-- the current view
type SessionView
  = Compose
  | UserDelegates
  | Connect
  | Browse
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
    , connector = Explorer.empty
    , browser = Explorer.empty
    , auth = auth
    , delegator = Delegator.fromActiveUser LoggedOut
    }
    ! [ Cmd.map AuthMsg authFx ]


update : Msg -> Session -> (Session, Cmd Msg)
update action session =
  case action of
    SetActiveUser activeUser ->
      updateViews
        { session
        | activeUser = activeUser
        , delegator = Delegator.fromActiveUser activeUser
        }

    GoCompose topicId ->
      setSessionTopic session Compose topicId

    GoExplore topicId ->
      case session.activeUser of
        LoggedOut ->
          setSessionTopic session Browse topicId
        LoggedIn _ ->
          setSessionTopic session Connect topicId

    GoBrowse topicId ->
      setSessionTopic session Browse topicId

    GoConnect topicId ->
      setSessionTopic session Connect topicId

    GoUserDelegates ->
      { session | currentView = UserDelegates } ! []

    Error err ->
      let
        msg = Debug.log "session error!" err
      in
        session ! []

    -- we only propagate topic, and not user, because
    -- the app shouldn't normally be switching users, and only
    -- does so in the current dev environment?
    -- TODO: reload views on user change/logout
    SetTopic topic ->
      updateViews { session | topic = Debug.log "new session topic" topic }

    ComposerMsg composerAction ->
      let
        (update, updateFx) =
          Composer.update composerAction session.topic session.composer
      in
        { session | composer = update }
        ! [ Cmd.map ComposerMsg updateFx ]

    ConnectorMsg msg ->
      let
        (update, updateCmd) =
          ExplorerUpdate.update {tid = session.topic.id} msg session.connector
      in
        { session | connector = update }
        ! [ Cmd.map ConnectorMsg updateCmd ]

    BrowserMsg msg ->
      let
        (update, updateCmd) =
          ExplorerUpdate.update {tid = session.topic.id} msg session.browser
      in
        { session | browser = update }
        ! [ Cmd.map BrowserMsg updateCmd ]

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
          in
            { session
            | delegator = update
            , activeUser =
                LoggedIn
                  { user
                  | trustees = update.saved
                  }
            }
            ! [ Cmd.map DelegatorMsg updateFx ]


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
  case session.activeUser of
    LoggedOut ->
      let
        (browser, browserCmd) =
          ExplorerUpdate.initAll session.topic.id Nothing

      in
        ( { session | browser = browser }
        , Cmd.map BrowserMsg browserCmd
        )

    LoggedIn user ->
      if session.topic.id < 0 then
        session ! []
      else
        let
          (composer, composerCmd) =
            Composer.init session.topic
          (connector, connectorCmd) =
            ExplorerUpdate.initConnected session.topic.id Nothing
          (browser, browserCmd) =
            ExplorerUpdate.initAll session.topic.id Nothing

        in
          ( { session
            | composer = composer
            , connector = connector
            , browser = browser
            }
          , Cmd.batch
            [ Cmd.map ComposerMsg composerCmd
            , Cmd.map ConnectorMsg connectorCmd
            , Cmd.map BrowserMsg browserCmd
            ]
          )


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
    Html.App.map transform <| Html.div [ class "session" ] sessionContent


-- used to help create the nav links
type alias SessionLinker a =
  { componentView : SessionView
  , sessionMsg : (Session -> Msg)
  , html : a -> Html Msg
  , getter : Session -> a
  }


-- builds a link, setting it to active if it matches the current view
buildComponentNav : SessionLinker a -> Session -> Html Msg
buildComponentNav {componentView, sessionMsg, html, getter} session =
  let
    classes =
      -- we need to examine the current view to determine whether this
      -- link is active or not
      -- this is the primary reason for this class; it is awkward to write 3
      -- if statements in sessionHeader
      [ ("session-link", True)
      , ("active", componentView == session.currentView)
      ]
  in
    Html.div
      [ Attributes.classList classes
      , Events.onClick <| sessionMsg session
      ]
      [ html <| getter session
      ]


composeLinker : Session -> Html Msg
composeLinker = buildComponentNav
  { componentView = Compose
  , sessionMsg = GoCompose << .id << .topic
  , html = Composer.navButton
  , getter = .composer
  }


browseLinker : Session -> Html Msg
browseLinker = buildComponentNav
  { componentView = Browse
  , sessionMsg = GoBrowse << .id << .topic
  , html = ExplorerView.allButton
  , getter = .browser
  }


connectLinker : Session -> Html Msg
connectLinker = buildComponentNav
  { componentView = Connect
  , sessionMsg = GoConnect << .id << .topic
  , html = ExplorerView.connectedButton
  , getter = .connector
  }


activeSubNav : Session -> Html Msg
activeSubNav session =
  Html.div
    [ class "session-overview" ]
    [ Html.h1
      [ class "topic-title" ]
      [ Html.text session.topic.text ]
    , Html.div
      [ class "session-links" ]
      [ connectLinker session
      , browseLinker session
      , composeLinker session
      ]
    ]


inactiveSubNav : Session -> Html Msg
inactiveSubNav session =
  Html.div
    [ class "session-overview" ]
    [ Html.h1
      [ class "topic-title" ]
      [ Html.text session.topic.text ]
    , Html.div
      [ class "session-links" ]
      [ browseLinker session ]
    ]


activeSessionContent : User -> Session -> List (Html Msg)
activeSessionContent user session =
  case session.currentView of

    Compose ->
      [ activeSubNav session
      , Html.div
        [ class "content" ]
        [ Html.App.map ComposerMsg (Composer.view session.composer) ]
      ]

    Browse ->
      [ activeSubNav session
      , Html.div
        [ class "content" ]
        [ ExplorerView.all session.browser
          |> Html.App.map BrowserMsg
        ]
      ]

    Connect ->
      [ activeSubNav session
      , Html.div
        [ class "content" ]
        [ ExplorerView.connected session.connector
          |> Html.App.map ConnectorMsg
        ]
      ]

    UserDelegates ->
      [ Html.div
        [ class "content" ]
        [ Html.App.map DelegatorMsg (Delegator.view user.emails session.delegator) ]
      ]

    Empty ->
      [ Html.div
        []
        [ Html.text "whoops, why we here?" ]
      ]


-- an inactiveSession should only route to browse
inactiveSessionContent : Session -> List (Html Msg)
inactiveSessionContent session =
  case session.currentView of

    Browse ->
      [ inactiveSubNav session
      , Html.div
        [ class "content" ]
        [ ExplorerView.all session.browser
          |> Html.App.map BrowserMsg
        ]
      ]

    _ ->
      [ inactiveSubNav session
      , Html.div
        [ class "content" ]
        [ Html.text "Sorry, you must be logged in to see this content" ]
      ]


navHeader : (Msg -> msg) -> Session -> List (Html msg)
navHeader transform {auth, activeUser} =
  List.append
    (Auth.view
      { transform = (transform << AuthMsg)
      , activeUser = activeUser
      }
      auth)
    (Delegator.navHeader (transform GoUserDelegates) activeUser)
