module Model.SurfacedOpinion.SurfacedOpinion exposing
  ( SurfacedOpinion(..)
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


import Model.SurfacedOpinion.Link as Link exposing (Link, UserLink)
import Model.Opinion.Metrics exposing (Metrics)
import Model.Opinion.Opinion as Opinion exposing (Opinion)
import Model.Path as Path


import Dict exposing (Dict)
import Json.Decode as Decode exposing ((:=))


type alias Qid = Int
type alias Tid = Int


type SurfacedOpinion
  = Connected Opinion Link
  | Unconnected Opinion


connectedDecoder : Decode.Decoder SurfacedOpinion
connectedDecoder =
  Decode.object2 connectedFromApi
    ("paths" := Decode.list Path.decoder)
    ("opinion" := Opinion.decoder)


unconnectedDecoder : Decode.Decoder SurfacedOpinion
unconnectedDecoder =
  Decode.map Unconnected Opinion.decoder


connectedFromApi : UserLink -> Opinion -> SurfacedOpinion
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


toDict : List SurfacedOpinion -> Dict Int SurfacedOpinion
toDict surfacedOpinions =
  Dict.fromList <| List.map keyPair surfacedOpinions


keyPair : SurfacedOpinion -> (Int, SurfacedOpinion)
keyPair c =
  (key c, c)


key : SurfacedOpinion -> Int
key = .id << opinion


influenceWithDefault : Int -> SurfacedOpinion -> Int
influenceWithDefault default =
  Opinion.influenceWithDefault default << opinion


influence : SurfacedOpinion -> Remote Int
influence = .influence << opinion


setInfluence : Remote Int -> SurfacedOpinion -> SurfacedOpinion
setInfluence =
  mapOpinion << Opinion.setInfluence


metrics : SurfacedOpinion -> Remote Metrics
metrics = .metrics << opinion


setMetrics : Remote Metrics -> SurfacedOpinion -> SurfacedOpinion
setMetrics =
  mapOpinion << Opinion.setMetrics


userLink : SurfacedOpinion -> Maybe UserLink
userLink surfacedOpinion =
  case surfacedOpinion of
    Unconnected _ ->
      Nothing

    Connected _ link ->
      link |> .userLink |> Just

-- lower is better
score : SurfacedOpinion -> Int
score =
  Maybe.withDefault 1000 << Maybe.map Link.score << userLink


opinion : SurfacedOpinion -> Opinion
opinion surfacedOpinion =
  case surfacedOpinion of
    Unconnected opinion ->
      opinion

    Connected opinion _ ->
      opinion


mapOpinion : (Opinion -> Opinion) -> SurfacedOpinion -> SurfacedOpinion
mapOpinion opinionFn surfacedOpinion =
  case surfacedOpinion of
    Unconnected opinion ->
      Unconnected (opinionFn opinion)

    Connected opinion link ->
      Connected (opinionFn opinion) link


countLinked : List SurfacedOpinion -> Int
countLinked = List.length << List.filterMap userLink
