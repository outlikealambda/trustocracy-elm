module World exposing
  ( Msg
  , World
  , init
  , view
  , subscriptions
  , update
  )

import Common.API as API
import Model.Topic as Topic exposing (Topic)
import Session exposing (Session)
import View.Header as HeaderView
import View.Topic as TopicView
import Utils.Cmd as CmdUtils


import Html exposing (Html, input, div, node, h1, text)
import Html.Attributes exposing (class, rel, href, placeholder, value, style)


type alias World =
  { session : Session
  , topics : List Topic
  , currentView : WorldView
  }


type WorldView = SessionView | TopicsView


type Msg
  = GoSession Session.Msg -- temp hack to let TopicView change the model state
  | GoHome
  | SessionMsg Session.Msg
  | TopicsLoadComplete (List Topic)
  | TopicsLoadFailed String
  | SNoOp String


subscriptions : World -> Sub Msg
subscriptions _ =
  Sub.batch
    [ Sub.map SessionMsg Session.subscriptions
    ]


init : String -> (World, Cmd Msg)
init path =
  initialModel


initialModel : (World, Cmd Msg)
initialModel =
  let
    (session, sessionFx) =
      Session.init
  in
    ( { topics = []
      , session = session
      , currentView = TopicsView
      }
    ! [ Cmd.map SessionMsg sessionFx
      , API.fetchAllTopics TopicsLoadFailed TopicsLoadComplete
      ]
    )


update : Msg -> World -> (World, Cmd Msg)
update message world =
  case message of
    GoSession sessionAction ->
      ( { world | currentView = SessionView }
      , CmdUtils.init <| SessionMsg sessionAction
      )

    GoHome ->
      { world | currentView = TopicsView } ! []

    SessionMsg sessionAction ->
      let
        (update, updateFx) =
          Session.update (Debug.log "session action" sessionAction) world.session
      in
        ( { world | session = update }
        , Cmd.map SessionMsg updateFx
        )

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
    [ Session.navHeader GoSession world.session
      |> HeaderView.view GoHome
    , div
      [ class "world" ]
      ( case (Debug.log "world view " world.currentView) of
        TopicsView ->
          [ TopicView.all (GoSession << Session.GoExplore) world.topics ]

        SessionView ->
          [ Session.view SessionMsg world.session ]
      )
    ]
