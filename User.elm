module User (Model, init, decoder) where

import Json.Decode as Json exposing ((:=))

type alias Model = {
  name: String,
  id: Int
}

init : String -> Int -> Model
init name userid =
  Model name userid

decoder : Json.Decoder Model
decoder =
  Json.object2 Model
    ("name" := Json.string)
    ("id" := Json.int)
