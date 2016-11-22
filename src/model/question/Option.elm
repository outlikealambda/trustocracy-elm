module Model.Question.Option exposing
  ( Option
  , decoder
  )


import Json.Decode as Decode


type alias Option =
  { id : Int
  , label : String
  }


decoder : Decode.Decoder Option
decoder =
  Decode.map2 Option
    (Decode.field "id" Decode.int)
    (Decode.field "label" Decode.string)
