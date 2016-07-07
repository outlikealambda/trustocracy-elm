module Model.Question.Option exposing
  ( Option
  , decoder
  )


import Json.Decode as Decode exposing ((:=))


type alias Option =
  { id : Int
  , label : String
  }


decoder : Decode.Decoder Option
decoder =
  Decode.object2 Option
    ("id" := Decode.int)
    ("label" := Decode.string)
