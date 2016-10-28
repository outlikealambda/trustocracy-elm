module Model.Connection.Connection exposing
  ( Connection
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


import Common.Extended as Extended exposing (Extended)
import Common.Remote as Remote exposing (Remote)


import Model.Connection.Details exposing (Details)
import Model.Connection.Link as Link exposing (Link, UserLink)
import Model.Connection.Metrics exposing (Metrics)
import Model.Opinion.Opinion as Opinion exposing (Opinion)
import Model.Path as Path


import Dict exposing (Dict)
import Json.Decode as Decode exposing ((:=))


type alias Qid = Int
type alias Tid = Int


type alias Connection = Extended Details Link


connectedDecoder : Decode.Decoder Connection
connectedDecoder =
  Decode.object2 connectedFromApi
    ("paths" := Decode.list Path.decoder)
    ("opinion" := Opinion.decoder)


unconnectedDecoder : Decode.Decoder Connection
unconnectedDecoder =
  Decode.object1 (Extended.Basic << detailsFromApi) Opinion.decoder


connectedFromApi : UserLink -> Opinion -> Connection
connectedFromApi paths opinion =
  Extended.Complex
    (detailsFromApi opinion)
    (linkFromApi paths)


linkFromApi : UserLink -> Link
linkFromApi userLink =
  { userLink = sortUserLink userLink
  , score = minScore 0 userLink
  }


detailsFromApi : Opinion -> Details
detailsFromApi opinion =
  { opinion = opinion
  , influence = Remote.NoRequest
  , metrics = Remote.NoRequest
  }


sortUserLink : UserLink -> UserLink
sortUserLink = List.sortBy .score


minScore : Int -> UserLink -> Int
minScore default =
  Maybe.withDefault default << List.minimum << List.map .score


toDict : List Connection -> Dict Int Connection
toDict connections =
  Dict.fromList <| List.map keyPair connections


keyPair : Connection -> (Int, Connection)
keyPair c =
  (key c, c)


key : Connection -> Int
key = .id << opinion


opinion : Connection -> Opinion
opinion = .opinion << unwrap


influence : Connection -> Remote Int
influence = .influence << unwrap


influenceWithDefault : Int -> Connection -> Int
influenceWithDefault default =
  Remote.withDefault default << influence


setInfluence : Remote Int -> Connection -> Connection
setInfluence influence =
  mapDetails (\b -> { b | influence = influence})


metrics : Connection -> Remote Metrics
metrics = .metrics << unwrap


setMetrics : Remote Metrics -> Connection -> Connection
setMetrics metrics =
  mapDetails (\b -> { b | metrics = metrics})


userLink : Connection -> Maybe UserLink
userLink connection =
  case connection of
    Extended.Basic _ ->
      Nothing

    Extended.Complex _ link ->
      link |> .userLink |> Just

-- lower is better
score : Connection -> Int
score =
  Maybe.withDefault 1000 << Maybe.map Link.score << userLink



unwrap : Connection -> Details
unwrap connection =
  case connection of
    Extended.Basic details ->
      details

    Extended.Complex details _ ->
      details


mapDetails : (Details -> Details) -> Connection -> Connection
mapDetails detailsFn connection =
  case connection of
    Extended.Basic details ->
      Extended.Basic (detailsFn details)

    Extended.Complex details link ->
      Extended.Complex (detailsFn details) link


countLinked : List Connection -> Int
countLinked = List.length << List.filterMap userLink
