module Model.Opinion.Metrics exposing
  ( Metrics
  , decoder
  )


import Json.Decode as Decode


type alias Metrics = List (Int, Float)


decoder : Decode.Decoder Metrics
decoder =
  Decode.list
    <| Decode.map2 (,)
      (Decode.field "questionId" Decode.int)
      (Decode.field "rating" Decode.float)
