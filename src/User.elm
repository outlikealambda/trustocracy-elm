module User
  ( User
  , decoder
  , encode
  , empty
  , isEmpty
  ) where

import Json.Decode as Decode exposing ((:=))
import Json.Encode as Encode
import Trustee exposing (Trustee)

type alias User =
  { name: String
  , id: Int
  , trustees: List Trustee
  }


empty : User
empty =
  { name = ""
  , id = -1
  , trustees = []
  }

decoder : Decode.Decoder User
decoder =
  Decode.object3 User
    ("name" := Decode.string)
    ("id" := Decode.int)
    ("trustees" := Decode.list Trustee.decoder)


encode : User -> Encode.Value
encode {name, id, trustees} =
  Encode.object
    [ ("name", Encode.string name)
    , ("id", Encode.int id)
    , ("trustees", Encode.list <| List.map Trustee.encoder trustees)
    ]


isEmpty : User -> Bool
isEmpty user =
  user.id == -1
