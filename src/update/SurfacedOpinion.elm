module Update.SurfacedOpinion exposing
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

import Model.SurfacedOpinion.SurfacedOpinion as SurfacedOpinion exposing (SurfacedOpinion)
import Model.Opinion.Metrics exposing (Metrics)


type alias Tid = Int -- Topic ID

type Msg
  = FetchedInfluence (Result String Int)
  | FetchedMetrics (Result String Metrics)


type alias Context =
  { tid : Tid
  }


update : Context -> Msg -> SurfacedOpinion -> (SurfacedOpinion, Cmd Msg)
update context msg surfacedOpinion =
  case msg of

    FetchedInfluence result ->
      case result of
        Ok influence ->
          SurfacedOpinion.setInfluence (Retrieved influence) surfacedOpinion ! []

        Err errorMsg ->
          let
            e = Debug.log "error fetching influence" errorMsg

          in
            SurfacedOpinion.setInfluence NoRequest surfacedOpinion ! []

    FetchedMetrics result ->
      case result of
        Ok metrics ->
          SurfacedOpinion.setMetrics (Retrieved metrics) surfacedOpinion ! []

        Err errorMsg ->
          let
            e = Debug.log "error fetching metrics" errorMsg

          in
            SurfacedOpinion.setMetrics NoRequest surfacedOpinion ! []


secondaryFetch : SurfacedOpinion -> (SurfacedOpinion, Cmd Msg)
secondaryFetch surfacedOpinion =
  let
    opinionId =
      SurfacedOpinion.key surfacedOpinion

    (influence, influenceCmd) =
      fetchInfluence opinionId (SurfacedOpinion.influence surfacedOpinion)

    (metrics, metricsCmd) =
      fetchMetrics opinionId


  in
    SurfacedOpinion.setInfluence influence surfacedOpinion
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
