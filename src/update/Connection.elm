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

import Model.Connection.Connection as Connection exposing (Connection)
import Model.Connection.Metrics exposing (Metrics)


type alias Tid = Int -- Topic ID

type Msg
  = FetchedInfluence (Result String Int)
  | FetchedMetrics (Result String Metrics)


type alias Context =
  { tid : Tid
  }


update : Context -> Msg -> Connection -> (Connection, Cmd Msg)
update context msg connection =
  case msg of

    FetchedInfluence result ->
      case result of
        Ok influence ->
          Connection.setInfluence (Retrieved influence) connection ! []

        Err errorMsg ->
          let
            e = Debug.log "error fetching influence" errorMsg

          in
            Connection.setInfluence NoRequest connection ! []

    FetchedMetrics result ->
      case result of
        Ok metrics ->
          Connection.setMetrics (Retrieved metrics) connection ! []

        Err errorMsg ->
          let
            e = Debug.log "error fetching metrics" errorMsg

          in
            Connection.setMetrics NoRequest connection ! []


secondaryFetch : Connection -> (Connection, Cmd Msg)
secondaryFetch connection =
  let
    opinionId =
      Connection.key connection

    (influence, influenceCmd) =
      fetchInfluence opinionId (Connection.influence connection)

    (metrics, metricsCmd) =
      fetchMetrics opinionId


  in
    Connection.setInfluence influence connection
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
