module Session
  ( Session
  , init
  , Action
    ( SetActiveUser
    , GoCompose
    , GoSurvey
    , GoBrowse
    , GoRead
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
import Opinion.Surveyor as Surveyor
import Opinion.Composer as Composer
import Opinion.Browser as Browser
import Opinion.Reader as Reader
import Routes


import Effects exposing (Effects)
import String
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
  , composer : Composer.Composer
  , surveyor : Surveyor.Surveyor
  , browser : Browser.Browser
  , reader : Reader.Reader
  }

type alias TopicId = Int
type alias OpinionId = Int


type Action
  -- exposed
  = SetActiveUser ActiveUser
  | GoCompose TopicId
  | GoSurvey TopicId
  | GoBrowse TopicId
  | GoRead TopicId OpinionId

  -- private
  | TopicMsg Topic.Action
  | PropagateTopic
  | ComposerMsg Composer.Action
  | SurveyorMsg Surveyor.Action
  | BrowserMsg Browser.Action
  | ReaderMsg Reader.Action
  | NoOp


-- the current view
-- we could reuse Routes.Route here, but it feels a little awkward
type SessionView
  = Compose
  | Survey
  | Browse
  | Read
  | Empty


init : Session
init =
  { activeUser = LoggedOut
  , topic = Topic.empty
  , currentView = Empty
  , composer = Composer.empty
  , surveyor = Surveyor.empty
  , browser = Browser.empty
  , reader = Reader.empty
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
      setSessionTopic session Survey topicId

    GoBrowse topicId ->
      setSessionTopic session Browse topicId

    GoRead topicId opinionId ->
      let
        (sessionUpdate, sessionUpdateFx) =
          setSessionTopic session Read topicId
        (reader, readerFx) =
          Reader.init session.activeUser opinionId
      in
        ( { sessionUpdate | reader = reader }
        , Effects.batch
          [ sessionUpdateFx
          , Effects.map ReaderMsg readerFx
          ]
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

    BrowserMsg browserAction ->
      let
        (update, updateFx) =
          Browser.update browserAction session.browser
      in
        ( { session | browser = update }
        , Effects.map BrowserMsg updateFx
        )

    ReaderMsg readerAction ->
      let
        (update, updateFx) =
          Reader.update readerAction session.reader
      in
        ( { session | reader = update }
        , Effects.map ReaderMsg updateFx )

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
      let
        (browserUpdate, browserUpdateFx) =
          Browser.init session.topic
      in
        ( { session | browser = browserUpdate }
        , Effects.map BrowserMsg browserUpdateFx )
    LoggedIn user ->
      let
        (composerUpdate, composerUpdateFx) =
          Composer.init user session.topic
        (surveyorUpdate, surveyorUpdateFx) =
          Surveyor.init user session.topic
        -- this is user independent, does it belong in Session?
        (browserUpdate, browserUpdateFx) =
          Browser.init session.topic
      in
        ( { session
          | composer = composerUpdate
          , surveyor = surveyorUpdate
          , browser = browserUpdate
          }
        , Effects.batch
          [ Effects.map ComposerMsg composerUpdateFx
          , Effects.map SurveyorMsg surveyorUpdateFx
          , Effects.map BrowserMsg browserUpdateFx
          ]
        )


view : Signal.Address Action -> Session -> Html
view address session =
  let
    (sessionHeader, sessionContent) =
      case session.activeUser of
        LoggedIn user ->
          ( activeSessionHeader session
          , activeSessionContent address session
          )
        LoggedOut ->
          ( inactiveSessionHeader session
          , inactiveSessionContent address session
          )
  in
    div
      [ class "session" ]
      [ sessionHeader
      , sessionContent
      ]


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


browseLinker : Session -> Html
browseLinker = buildNavLink
  { routeView = Browse
  , buildRoute = Routes.Browse
  , makeHtml = Browser.navButton
  , getter = .browser
  }


activeSessionHeader : Session -> Html
activeSessionHeader session =
  div
    [ class "session-overview" ]
    [ h1 [ class "topic-title" ] [ text session.topic.text ]
    , div
      [ class "session-links" ]
      [ connectLinker session
      , browseLinker session
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
      [ browseLinker session ]
    ]


activeSessionContent : Signal.Address Action -> Session -> Html
activeSessionContent address session =
  let
    opinionRouter = Routes.Read session.topic.id
  in
    case session.currentView of

      Survey ->
        div
          [ class "content" ]
          <| Surveyor.view
            { address = Signal.forwardTo address SurveyorMsg
            , routeBuilder = opinionRouter
            }
            session.surveyor

      Compose ->
        div
          [ class "content" ]
          [ Composer.view (Signal.forwardTo address ComposerMsg) session.composer ]

      Browse ->
        div
          [ class "content" ]
          [ Browser.view opinionRouter session.browser ]

      Read ->
        div
          [ class "content" ]
          [ Reader.view session.reader ]

      Empty ->
        div [] [ text "whoops, why we here?" ]


-- an inactiveSession should only route to browse
inactiveSessionContent : Signal.Address Action -> Session -> Html
inactiveSessionContent address session =
  case session.currentView of
    Browse ->
      div
        [ class "content" ]
        [ Browser.view (Routes.Read session.topic.id) session.browser ]
    _ ->
      div [] [ text "Sorry, you must be logged in to see this content" ]
