module User
  ( User
  , decoder
  ) where

import Json.Decode as Json exposing ((:=))


type alias User = {
  name: String,
  id: Int
}


decoder : Json.Decoder User
decoder =
  Json.object2 User
    ("name" := Json.string)
    ("id" := Json.int)
