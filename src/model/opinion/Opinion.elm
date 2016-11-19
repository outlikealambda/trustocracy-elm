module Model.Opinion.Opinion exposing
  ( Opinion
  , decoder
  , setMetrics
  , setInfluence
  , influenceWithDefault
  )


import Common.Remote as Remote exposing (Remote)
import Model.Opinion.Metrics exposing (Metrics)
import Model.Qualifications as Qualifications exposing (Qualifications)
import Model.Trustee as Trustee exposing (Trustee)
import Utils.String as StringUtils


import Date exposing (Date)
import Json.Decode as Decode exposing ((:=))
import Time


type alias Opinion =
  { id : Int
  , text : String
  , author : Trustee
  , qualifications : Qualifications
  , created : Date

  -- derived
  , snippet : String
  , preview : String
  , expanded : Bool
  , fetched : Bool

  -- secondary fetch
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
  Decode.object5 fromApi
    ("id" := Decode.int)
    ("text" := Decode.string)
    ("author" := Trustee.decoder)
    (Decode.oneOf
      [ "qualifications" := Qualifications.decoder
      , Decode.succeed Qualifications.empty
      ]
    )
    ("created" := Decode.float)


fromApi : Int -> String -> Trustee -> Qualifications -> Float -> Opinion
fromApi id text author qualifications created =
  { id = id
  , text = text
  , author = author
  , qualifications = qualifications
  , created = Date.fromTime <| Time.second * created
  , fetched = True
  , snippet = StringUtils.snippetize 300 text
  , preview = StringUtils.snippetize 300 text
  , expanded = False
  , influence = Remote.NoRequest
  , metrics = Remote.NoRequest
  }
