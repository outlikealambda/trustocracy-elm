module Model.Opinion.Opinion exposing
  ( Opinion
  , decoder
  , setMetrics
  , setInfluence
  , influenceWithDefault
  )


import Common.Remote as Remote exposing (Remote)
import Model.Extend.Identified exposing (Identified)
import Model.Opinion.Metrics exposing (Metrics)
import Model.Opinion.Record as Record exposing (Record)
import Json.Decode as Decode


type alias Opinion =
  Identified
    ( Record
      { influence : Remote Int
      , metrics : Remote Metrics
      }
    )


influenceWithDefault : Int -> Opinion -> Int
influenceWithDefault default =
  Remote.withDefault default << .influence


setInfluence : Remote Int -> Opinion -> Opinion
setInfluence influence opinion =
  { opinion | influence = influence }


setMetrics : Remote Metrics -> Opinion -> Opinion
setMetrics metrics opinion =
  { opinion | metrics = metrics }


decoder : Decode.Decoder Opinion
decoder =
  Decode.map2 fromApi
    (Decode.field "id" Decode.int)
    Record.decoder


fromApi : Int -> Record a -> Opinion
fromApi id record =
  { id = id
  , text = record.text
  , author = record.author
  , qualifications = record.qualifications
  , created = record.created
  , fetched = record.fetched
  , snippet = record.snippet
  , preview = record.preview
  , expanded = record.expanded
  , influence = Remote.NoRequest
  , metrics = Remote.NoRequest
  }
