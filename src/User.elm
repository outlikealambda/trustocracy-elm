module User
  ( User
  , decoder
  , empty
  ) where

import Json.Decode as Json exposing ((:=))


type alias User =
  { name: String
  , id: Int
  }

empty : User
empty =
  { name = ""
  , id = -1
  }

decoder : Json.Decoder User
decoder =
  Json.object2 User
    ("name" := Json.string)
    ("id" := Json.int)
