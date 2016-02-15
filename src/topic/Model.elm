module Topic.Model
  ( Topic
  , Action
    ( FetchComplete )
  , empty
  , init
  , update
  , fetch
  , fetchAll
  , justCompleted
  ) where


import String
import Json.Decode as Json exposing ((:=))
import Http
import Task exposing (Task)
import Effects exposing (Effects)


type alias Topic =
  { id : Int
  , text : String
  , isComplete : Bool
  }


type Action
  = FetchComplete (Maybe Topic)


empty : Topic
empty = Topic -1 "" False


init : Int -> (Topic, Effects Action)
init topicId =
  let initFx =
    fetch topicId
      |> Task.toMaybe
      |> Task.map FetchComplete
      |> Effects.task
  in
    ( { id = topicId
      , text = ""
      , isComplete = False
      }
    , initFx
    )


update : Action -> Topic -> (Topic, Effects Action)
update action topic =
  case action of

    FetchComplete maybeTopic ->
      let model =
        Maybe.withDefault empty maybeTopic
      in
        ( { model | isComplete = True }
        , Effects.none
        )


decoder : Json.Decoder Topic
decoder = Json.object2
  (\id text -> Topic id text False)
  ("id" := Json.int)
  ("text" := Json.string)


listDecoder : Json.Decoder (List Topic)
listDecoder =
  Json.list decoder


fetch : Int -> Task Http.Error Topic
fetch topicId =
  topicUrl topicId
    |> Http.get decoder


topicUrl : Int -> String
topicUrl topicId =
  "http://localhost:3714/api/topic/" ++ toString topicId


fetchAll : Task Http.Error (List Topic)
fetchAll =
  topicsUrl
    |> Http.get listDecoder


topicsUrl : String
topicsUrl =
  "http://localhost:3714/api/topic"


justCompleted : Topic -> Topic -> Bool
justCompleted a b =
  a.id /= b.id
