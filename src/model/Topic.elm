module Model.Topic exposing
  ( Topic
  , empty
  , decoder
  )


import Json.Decode as Decode exposing ((:=))


type alias Topic =
  { id : Int
  , text : String
  , isComplete : Bool
  }

empty : Topic
empty = Topic -1 "" False


decoder : Decode.Decoder Topic
decoder = Decode.object2
  (\id text -> Topic id text False)
  ("id" := Decode.int)
  ("text" := Decode.string)


topicUrl : Int -> String
topicUrl topicId =
  "http://localhost:3714/api/topic/" ++ toString topicId


topicsUrl : String
topicsUrl =
  "http://localhost:3714/api/topic"
