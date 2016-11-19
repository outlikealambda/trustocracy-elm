module Model.Opinion.Record exposing
  ( Record
  , decoder
  )


import Model.Qualifications as Qualifications exposing (Qualifications)
import Model.Trustee as Trustee exposing (Trustee)
import Utils.String as StringUtils


import Date exposing (Date)
import Json.Decode as Decode exposing ((:=))
import Time


-- even though record is only ever instantiated with an empty a,
-- we need it to create functions that work with records with contributed
-- fields; otherwise the type matching will fail.
type alias Record a =

  -- from API
  { a
  | text : String
  , author : Trustee
  , qualifications : Qualifications
  , created : Date

  -- derived
  , snippet : String
  , preview : String
  , expanded : Bool
  , fetched : Bool
  }


decoder : Decode.Decoder (Record {})
decoder =
  Decode.object4 fromApi
    ("text" := Decode.string)
    ("author" := Trustee.decoder)
    (Decode.oneOf
      [ "qualifications" := Qualifications.decoder
      , Decode.succeed Qualifications.empty
      ]
    )
    ("created" := Decode.float)


fromApi : String -> Trustee -> Qualifications -> Float -> Record {}
fromApi text author qualifications created =
  { text = text
  , author = author
  , qualifications = qualifications
  , created = Date.fromTime <| Time.second * created
  , fetched = True
  , snippet = StringUtils.snippetize 300 text
  , preview = StringUtils.snippetize 300 text
  , expanded = False
  }
