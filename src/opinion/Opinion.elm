module Opinion.Opinion
  ( Opinion
  , empty
  , decoder
  , encode
  ) where


import Trustee exposing (Trustee)
import Qualifications as Qualifications exposing (Qualifications)


import Json.Decode as Decode exposing ((:=))
import Json.Encode as Encode


type alias Opinion =

  -- from API
  { id : Int
  , text : String
  , influence : Int
  , opiner : Trustee
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
  , opiner = Trustee.empty
  , qualifications = Qualifications.empty
  , snippet = ""
  , expanded = False
  , fetched = False
  }


decoder : Decode.Decoder Opinion
decoder =
  Decode.object5 fromApi
    ( "id" := Decode.int )
    ( "text" := Decode.string )
    ( "influence" := Decode.int )
    ( "opiner" := Trustee.decoder )
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
    , ("opiner", Trustee.encoder opinion.opiner)
    , ("qualifications", Qualifications.encode opinion.qualifications)
    ]


fromApi : Int -> String -> Int -> Trustee -> Qualifications -> Opinion
fromApi id text influence opiner qualifications =
  { empty
  | id = id
  , text = text
  , influence = influence
  , opiner = opiner
  , qualifications = qualifications
  , fetched = True
  }
