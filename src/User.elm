module User
  ( User
  , decoder
  , encode
  , empty
  , isEmpty
  ) where

import Json.Decode as Decode exposing ((:=))
import Json.Encode as Encode


type alias User =
  { name: String
  , id: Int
  }

empty : User
empty =
  { name = ""
  , id = -1
  }

decoder : Decode.Decoder User
decoder =
  Decode.object2 User
    ("name" := Decode.string)
    ("id" := Decode.int)


encode : User -> Encode.Value
encode {name, id} =
  Encode.object
    [ ("name", Encode.string name)
    , ("id", Encode.int id)
    ]


isEmpty : User -> Bool
isEmpty user =
  user.id == -1
