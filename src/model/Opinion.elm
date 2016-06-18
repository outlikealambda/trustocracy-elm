module Model.Opinion exposing
  ( Opinion
  , empty
  , decoder
  , encode
  )


import Model.Qualifications as Qualifications exposing (Qualifications)
import Model.Trustee as Trustee exposing (Trustee)
import Utils.String as StringUtils


import Json.Decode as Decode exposing ((:=))
import Json.Encode as Encode


type alias Opinion =

  -- from API
  { id : Int
  , text : String
  , influence : Int
  , author : Trustee
  , qualifications : Maybe Qualifications

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
  , qualifications = Nothing
  , snippet = ""
  , expanded = False
  , fetched = False
  }


decoder : Decode.Decoder Opinion
decoder =
  Decode.object5 fromApi
    ("id" := Decode.int)
    ("text" := Decode.string)
    ("influence" := Decode.int)
    ("author" := Trustee.decoder)
    (Decode.maybe <| "qualifications" := Qualifications.decoder)


encode : Opinion -> Encode.Value
encode opinion =
  Encode.object
    [ ("id", Encode.int opinion.id)
    , ("text", Encode.string opinion.text)
    , ("influence", Encode.int opinion.influence)
    ]


fromApi : Int -> String -> Int -> Trustee -> Maybe Qualifications -> Opinion
fromApi id text influence author qualifications =
  { empty
  | id = id
  , text = text
  , influence = influence
  , author = author
  , qualifications = qualifications
  , fetched = True
  , snippet = StringUtils.snippetize 100 text
  }
