module Update.Connection exposing
  ( Msg
  , update
  , secondaryFetch
  )

import Common.API as API
import Common.Remote as Remote exposing
  ( Remote
    ( NoRequest
    , Requested
    , Retrieved
    )
  )

import Model.TopicOpinion.TopicOpinion as TopicOpinion exposing (TopicOpinion)
import Model.Opinion.Metrics exposing (Metrics)


type alias Tid = Int -- Topic ID

type Msg
  = FetchedInfluence (Result String Int)
  | FetchedMetrics (Result String Metrics)


type alias Context =
  { tid : Tid
  }


update : Context -> Msg -> TopicOpinion -> (TopicOpinion, Cmd Msg)
update context msg connection =
  case msg of

    FetchedInfluence result ->
      case result of
        Ok influence ->
          TopicOpinion.setInfluence (Retrieved influence) connection ! []

        Err errorMsg ->
          let
            e = Debug.log "error fetching influence" errorMsg

          in
            TopicOpinion.setInfluence NoRequest connection ! []

    FetchedMetrics result ->
      case result of
        Ok metrics ->
          TopicOpinion.setMetrics (Retrieved metrics) connection ! []

        Err errorMsg ->
          let
            e = Debug.log "error fetching metrics" errorMsg

          in
            TopicOpinion.setMetrics NoRequest connection ! []


secondaryFetch : TopicOpinion -> (TopicOpinion, Cmd Msg)
secondaryFetch connection =
  let
    opinionId =
      TopicOpinion.key connection

    (influence, influenceCmd) =
      fetchInfluence opinionId (TopicOpinion.influence connection)

    (metrics, metricsCmd) =
      fetchMetrics opinionId


  in
    TopicOpinion.setInfluence influence connection
    ! [ influenceCmd, metricsCmd ]


fetchInfluence : Int -> Remote Int -> (Remote Int, Cmd Msg)
fetchInfluence key influence =
  case influence of
    NoRequest ->
      ( Requested
      , API.fetchInfluence
        (FetchedInfluence << Err)
        (FetchedInfluence << Ok)
        key
      )
    _ ->
      ( influence
      , Cmd.none
      )


fetchMetrics : Int -> (Remote Metrics, Cmd Msg)
fetchMetrics key =
  ( Requested
  , API.fetchMetrics
    (FetchedMetrics << Err)
    (FetchedMetrics << Ok)
    key
  )
