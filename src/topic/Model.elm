module Topic.Model
  ( Topic
  , empty
  , get
  , getAll
  ) where


import String
import Json.Decode as Json exposing ((:=))
import Http
import Task exposing (Task)


type alias Topic =
  { id : Int
  , text : String
  }


empty : Topic
empty = Topic -1 ""


decoder : Json.Decoder Topic
decoder = Json.object2 Topic
  ("id" := Json.int)
  ("text" := Json.string)


listDecoder : Json.Decoder (List Topic)
listDecoder =
  Json.list decoder


get : Int -> Task Http.Error Topic
get topicId =
  topicUrl topicId
    |> Http.get decoder


topicUrl : Int -> String
topicUrl topicId =
  "http://localhost:3714/api/topic/" ++ toString topicId


getAll : Task Http.Error (List Topic)
getAll =
  topicsUrl
    |> Http.get listDecoder


topicsUrl : String
topicsUrl =
  "http://localhost:3714/api/topic"
