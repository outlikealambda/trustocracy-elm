module Model.Place exposing (..)

import Json.Encode as Encode
import Json.Decode as Decode exposing ((:=))

type alias Place =
  { name : String
  , id : Int
  , country : String
  , city : String
  , postal : String
  }

createEmpty : Place
createEmpty =
  { name = ""
  , id = -1
  , country = ""
  , city = ""
  , postal = ""
  }

encoder : Place -> Encode.Value
encoder place =
  Encode.object
    [ ("name", Encode.string place.name)
    , ("country", Encode.string place.country)
    , ("city", Encode.string place.city)
    , ("postal", Encode.string place.postal)
    ]

decoder : Decode.Decoder Place
decoder =
  Decode.object5
    Place
    ("name" := Decode.string)
    ("id" := Decode.int)
    ("country" := Decode.string)
    ("city" := Decode.string)
    ("postal" := Decode.string)

removalDecoder : Decode.Decoder Int
removalDecoder =
  Decode.int
