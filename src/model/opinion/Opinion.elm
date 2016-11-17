module Model.Opinion.Opinion exposing
  ( Opinion
  , decoder
  , setMetrics
  , setInfluence
  , influenceWithDefault
  )


import Common.Remote as Remote exposing (Remote)
import Model.Extend.Identified as Identified exposing (Identified)
import Model.Opinion.Metrics exposing (Metrics)
import Model.Opinion.Record exposing (Record)
import Model.Qualifications as Qualifications exposing (Qualifications)
import Model.Trustee as Trustee exposing (Trustee)
import Utils.String as StringUtils


import Date exposing (Date)
import Json.Decode as Decode exposing ((:=))
import Time


type alias OpinionRecord = Identified (Record {})


type alias Opinion =
  { record : OpinionRecord
  , influence : Remote Int
  , metrics : Remote Metrics
  }


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
  Decode.map fromApi recordDecoder


recordDecoder : Decode.Decoder OpinionRecord
recordDecoder =
  Decode.object5 recordFromApi
    ("id" := Decode.int)
    ("text" := Decode.string)
    ("author" := Trustee.decoder)
    (Decode.oneOf
      [ "qualifications" := Qualifications.decoder
      , Decode.succeed Qualifications.empty
      ]
    )
    ("created" := Decode.float)


fromApi : OpinionRecord -> Opinion
fromApi record =
  { record = record
  , influence = Remote.NoRequest
  , metrics = Remote.NoRequest
  }


recordFromApi : Int -> String -> Trustee -> Qualifications -> Float -> OpinionRecord
recordFromApi id text author qualifications created =
  { id = id
  , text = text
  , author = author
  , qualifications = qualifications
  , created = Date.fromTime <| Time.second * created
  , fetched = True
  , snippet = StringUtils.snippetize 300 text
  , preview = StringUtils.snippetize 300 text
  , expanded = False
  }
