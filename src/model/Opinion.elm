module Model.Opinion exposing
  ( Opinion
  , empty
  , decoder
  , encode
  )


import Model.Qualifications as Qualifications exposing (Qualifications)
import Model.Trustee as Trustee exposing (Trustee)
import Utils.String as StringUtils


import Date exposing (Date)
import Json.Decode as Decode exposing ((:=))
import Json.Encode as Encode
import Time


type alias Opinion =

  -- from API
  { id : Int
  , text : String
  , influence : Int
  , author : Trustee
  , qualifications : Qualifications
  , created : Date

  -- derived
  , snippet : String
  , expanded : Bool
  , fetched : Bool
  }


empty : Opinion
empty =
  { id = -1
  , text = ""
  , influence = -1
  , author = Trustee.empty
  , qualifications = []
  , created = Date.fromTime 0
  , snippet = ""
  , expanded = False
  , fetched = False
  }


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


encode : Opinion -> Encode.Value
encode opinion =
  Encode.object
    [ ("id", Encode.int opinion.id)
    , ("text", Encode.string opinion.text)
    , ("influence", Encode.int opinion.influence)
    ]


fromApi : Int -> String -> Int -> Trustee -> Qualifications -> Float -> Opinion
fromApi id text influence author qualifications created =
  { empty
  | id = id
  , text = text
  , influence = influence
  , author = author
  , qualifications = qualifications
  , created = Date.fromTime <| Time.second * created
  , fetched = True
  , snippet = StringUtils.snippetize 100 text
  }
