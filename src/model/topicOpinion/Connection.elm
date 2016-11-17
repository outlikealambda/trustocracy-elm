module Model.TopicOpinion.Connection exposing
  ( TopicOpinion(..)
  , connectedDecoder
  , unconnectedDecoder
  , toDict
  , key
  , influence
  , influenceWithDefault
  , opinion
  , userLink
  , setInfluence
  , setMetrics
  , countLinked
  , score
  )


import Common.Remote as Remote exposing (Remote)


import Model.TopicOpinion.Link as Link exposing (Link, UserLink)
import Model.TopicOpinion.Metrics exposing (Metrics)
import Model.Opinion.Opinion as Opinion exposing (Opinion)
import Model.Path as Path


import Dict exposing (Dict)
import Json.Decode as Decode exposing ((:=))


type alias Qid = Int
type alias Tid = Int


type TopicOpinion
  = Connected Opinion Link
  | Unconnected Opinion


connectedDecoder : Decode.Decoder TopicOpinion
connectedDecoder =
  Decode.object2 connectedFromApi
    ("paths" := Decode.list Path.decoder)
    ("opinion" := Opinion.decoder)


unconnectedDecoder : Decode.Decoder TopicOpinion
unconnectedDecoder =
  Decode.map Unconnected Opinion.decoder


connectedFromApi : UserLink -> Opinion -> TopicOpinion
connectedFromApi paths opinion =
  Connected opinion (linkFromApi paths)


linkFromApi : UserLink -> Link
linkFromApi userLink =
  { userLink = sortUserLink userLink
  , score = minScore 0 userLink
  }


sortUserLink : UserLink -> UserLink
sortUserLink = List.sortBy .score


minScore : Int -> UserLink -> Int
minScore default =
  Maybe.withDefault default << List.minimum << List.map .score


toDict : List TopicOpinion -> Dict Int TopicOpinion
toDict topicOpinions =
  Dict.fromList <| List.map keyPair topicOpinions


keyPair : TopicOpinion -> (Int, TopicOpinion)
keyPair c =
  (key c, c)


key : TopicOpinion -> Int
key = .id << .record << opinion


influenceWithDefault : Int -> TopicOpinion -> Int
influenceWithDefault default =
  Opinion.influenceWithDefault default << opinion


influence : TopicOpinion -> Remote Int
influence = .influence << opinion


setInfluence : Remote Int -> TopicOpinion -> TopicOpinion
setInfluence =
  mapOpinion << Opinion.setInfluence


metrics : TopicOpinion -> Remote Metrics
metrics = .metrics << opinion


setMetrics : Remote Metrics -> TopicOpinion -> TopicOpinion
setMetrics =
  mapOpinion << Opinion.setMetrics


userLink : TopicOpinion -> Maybe UserLink
userLink topicOpinion =
  case topicOpinion of
    Unconnected _ ->
      Nothing

    Connected _ link ->
      link |> .userLink |> Just

-- lower is better
score : TopicOpinion -> Int
score =
  Maybe.withDefault 1000 << Maybe.map Link.score << userLink


opinion : TopicOpinion -> Opinion
opinion topicOpinion =
  case topicOpinion of
    Unconnected opinion ->
      opinion

    Connected opinion _ ->
      opinion


mapOpinion : (Opinion -> Opinion) -> TopicOpinion -> TopicOpinion
mapOpinion opinionFn topicOpinion =
  case topicOpinion of
    Unconnected opinion ->
      Unconnected (opinionFn opinion)

    Connected opinion link ->
      Connected (opinionFn opinion) link


countLinked : List TopicOpinion -> Int
countLinked = List.length << List.filterMap userLink
