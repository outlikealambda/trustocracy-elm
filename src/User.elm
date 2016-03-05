module User
  ( User
  , decoder
  , empty
  , isEmpty
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

isEmpty : User -> Bool
isEmpty user =
  user.id == -1
