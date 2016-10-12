module Model.Connection.Metrics exposing
  ( Metrics
  , decoder
  )


import Json.Decode as Decode exposing ((:=))


type alias Metrics = List (Int, Float)


decoder : Decode.Decoder Metrics
decoder =
  Decode.list
    <| Decode.object2 (,)
      ("questionId" := Decode.int)
      ("rating" := Decode.float)
