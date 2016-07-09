module Model.Opinion.Opinion exposing
  ( Opinion
  , decoder
  )


import Model.Extend.Identified as Identified exposing (Identified)
import Model.Opinion.Record exposing (Record)
import Model.Qualifications as Qualifications exposing (Qualifications)
import Model.Trustee as Trustee exposing (Trustee)
import Utils.String as StringUtils


import Date exposing (Date)
import Json.Decode as Decode exposing ((:=))
import Time


type alias Opinion = Identified (Record {})


decoder : Decode.Decoder Opinion
decoder =
  Decode.object6 fromApi
    ("id" := Decode.int)
    ("text" := Decode.string)
    ("influence" := Decode.int)
    ("author" := Trustee.decoder)
    (Decode.oneOf
      [ "qualifications" := Qualifications.decoder
      , Decode.succeed Qualifications.empty
      ]
    )
    ("created" := Decode.float)



fromApi : Int -> String -> Int -> Trustee -> Qualifications -> Float -> Opinion
fromApi id text influence author qualifications created =
  { id = id
  , text = text
  , influence = influence
  , author = author
  , qualifications = qualifications
  , created = Date.fromTime <| Time.second * created
  , fetched = True
  , snippet = StringUtils.snippetize 100 text
  , expanded = False
  }
