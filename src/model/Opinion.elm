module Model.Opinion exposing
  ( Opinion
  , empty
  , decoder
  , encode
  )


import Model.Qualifications as Qualifications exposing (Qualifications)
import Utils.String as StringUtils


import Json.Decode as Decode exposing ((:=))
import Json.Encode as Encode


type alias Opinion =

  -- from API
  { id : Int
  , text : String
  , influence : Int
  , qualifications : Qualifications

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
  , qualifications = Qualifications.empty
  , snippet = ""
  , expanded = False
  , fetched = False
  }


decoder : Decode.Decoder Opinion
decoder =
  Decode.object4 fromApi
    ( "id" := Decode.int )
    ( "text" := Decode.string )
    ( "influence" := Decode.int )
    ( Decode.oneOf
      [ "qualifications" := Qualifications.decoder
      , Decode.succeed Qualifications.empty
      ]
    )


encode : Opinion -> Encode.Value
encode opinion =
  Encode.object
    [ ("id", Encode.int opinion.id)
    , ("text", Encode.string opinion.text)
    , ("influence", Encode.int opinion.influence)
    , ("qualifications", Qualifications.encode opinion.qualifications)
    ]


fromApi : Int -> String -> Int -> Qualifications -> Opinion
fromApi id text influence qualifications =
  { empty
  | id = id
  , text = text
  , influence = influence
  , qualifications = qualifications
  , fetched = True
  , snippet = StringUtils.snippetize 100 text
  }
