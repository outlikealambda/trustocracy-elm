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
import Task

import Common.API as API
import Header
import Location
import Routes exposing (Route)
import Session exposing (Session)
import Topic.Model exposing (Topic)
import Topic.View


type alias World =
  { route : Route
  , session : Session
  , topics : List Topic
  }


type Msg
  = SessionMsg Session.Msg
  | TopicsLoad (List Topic)
  --| RouterAction (TransitRouter.Action Routes.Route)
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
      ( world, Cmd.none )


init : String -> (World, Cmd Msg)
init path =
  let
    (initialWorld, initialWorldFx) =
      initialModel
    (world, fx) =
      mountRoute (Routes.decode path) initialWorld
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
      }
    , Cmd.map SessionMsg sessionFx )


update : Msg -> World -> (World, Cmd Msg)
update message world =
  case message of
    SessionMsg sessionAction ->
      let
        (update, updateFx) =
          Session.update sessionAction world.session
      in
        ( { world | session = update }
        , Cmd.map SessionMsg updateFx
        )

    SetPath path ->
      ( world, Location.setPath path )

    PathUpdated path ->
      mountRoute (Routes.decode path) world

    TopicsLoad topics ->
      ( { world | topics = topics }
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
  Task.succeed sessionAction
    |> Task.perform (\_ -> SNoOp "error performing successful task") SessionMsg


view : World -> Html Msg
view world =
  div []
    [ Header.view
      (\_ -> SNoOp "FIXME")
      <| Session.navHeader SessionMsg world.session
    , div
      [ class "world" ]
      ( case world.route of

        Routes.Topics ->
          [ Topic.View.viewAll (\_ -> SNoOp "FIXME") world.topics ]

        Routes.Home ->
          [ Topic.View.viewAll (\_ -> SNoOp "FIXME") world.topics ]

        Routes.Survey _ ->
          [ Session.view SessionMsg world.session ]

        Routes.Compose _ ->
          [ Session.view SessionMsg world.session ]

        Routes.Read _ _ ->
          [ Session.view SessionMsg world.session ]

        Routes.UserDelegates ->
          [ Session.view SessionMsg world.session ]

        Routes.EmptyRoute ->
          [ text "" ]
      )
    ]
