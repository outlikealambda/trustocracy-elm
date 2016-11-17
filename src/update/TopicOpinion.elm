module Update.TopicOpinion exposing
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
update context msg topicOpinion =
  case msg of

    FetchedInfluence result ->
      case result of
        Ok influence ->
          TopicOpinion.setInfluence (Retrieved influence) topicOpinion ! []

        Err errorMsg ->
          let
            e = Debug.log "error fetching influence" errorMsg

          in
            TopicOpinion.setInfluence NoRequest topicOpinion ! []

    FetchedMetrics result ->
      case result of
        Ok metrics ->
          TopicOpinion.setMetrics (Retrieved metrics) topicOpinion ! []

        Err errorMsg ->
          let
            e = Debug.log "error fetching metrics" errorMsg

          in
            TopicOpinion.setMetrics NoRequest topicOpinion ! []


secondaryFetch : TopicOpinion -> (TopicOpinion, Cmd Msg)
secondaryFetch topicOpinion =
  let
    opinionId =
      TopicOpinion.key topicOpinion

    (influence, influenceCmd) =
      fetchInfluence opinionId (TopicOpinion.influence topicOpinion)

    (metrics, metricsCmd) =
      fetchMetrics opinionId


  in
    TopicOpinion.setInfluence influence topicOpinion
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
