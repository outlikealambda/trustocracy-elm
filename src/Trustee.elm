module Trustee
  ( Trustee
  , decoder
  , encoder
  ) where


import Json.Decode as Decode exposing ((:=))
import Json.Encode as Encode


type alias Trustee =
  { name: String
  , id: Int
  , relationship: String
  }


decoder : Decode.Decoder Trustee
decoder =
  Decode.object3 Trustee
    ("name" := Decode.string)
    ("id" := Decode.int)
    ("relationship" := Decode.string)


encoder : Trustee -> Encode.Value
encoder {name, id, relationship} =
  Encode.object
    [ ("name", Encode.string name)
    , ("id", Encode.int id)
    , ("relationship", Encode.string relationship)
    ]
