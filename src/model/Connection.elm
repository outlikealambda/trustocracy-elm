module Model.Connection exposing
  ( Connection
  , Basic
  , Linked
  , connectedDecoder
  , unconnectedDecoder
  , toDict
  , key
  , influence
  , assessor
  , opinion
  , inflation
  , userLink
  , setInfluence
  , setAssessor
  , collapse
  , expand
  , countLinked
  , either
  )


import Common.Remote as Remote exposing (Remote)


import Model.Extend.Expandable as Expandable exposing (Expandable)
import Model.Opinion.Opinion as Opinion exposing (Opinion)
import Model.Path as Path exposing (Path)
import Model.Question.Assessor as Assessor exposing (Assessor)


import Dict exposing (Dict)
import Json.Decode as Decode exposing ((:=))


type alias Qid = Int
type alias Tid = Int


type alias Basic =
  Expandable
    { opinion : Opinion
    , influence : Remote Int
    , assessor : Maybe Assessor
    }


type alias Link =
  { userLink : UserLink
  , score : Int
  }


type alias Linked =
  { basic : Basic
  , link : Link
  }


type Connection
  = Connected Linked
  | Unconnected Basic


type alias UserLink = List Path


connectedDecoder : Decode.Decoder Connection
connectedDecoder =
  Decode.object2 connectedFromApi
    ("paths" := Decode.list Path.decoder)
    ("opinion" := Opinion.decoder)


unconnectedDecoder : Decode.Decoder Connection
unconnectedDecoder =
  Decode.object1 (Unconnected << basicFromApi) Opinion.decoder


connectedFromApi : List Path -> Opinion -> Connection
connectedFromApi paths opinion =
  Connected
    { basic = basicFromApi opinion
    , link = linkFromApi paths
    }


linkFromApi : List Path -> Link
linkFromApi paths =
  { userLink = sortPaths paths
  , score = minScore 0 paths
  }


basicFromApi : Opinion -> Basic
basicFromApi opinion =
  { opinion = opinion
  , influence = Remote.NoRequest
  , assessor = Nothing
  , inflation = Expandable.Collapsed
  }


sortPaths : List Path -> List Path
sortPaths = List.sortBy .score


minScore : Int -> List Path -> Int
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


setInfluence : Remote Int -> Connection -> Connection
setInfluence influence =
  mapBasic (\b -> { b | influence = influence})


assessor : Connection -> Maybe Assessor
assessor = .assessor << unwrap


setAssessor : Maybe Assessor -> Connection -> Connection
setAssessor assessor =
  mapBasic (\b -> { b | assessor = assessor})


inflation : Connection -> Expandable.Inflation
inflation = .inflation << unwrap


userLink : Connection -> Maybe UserLink
userLink connection =
  case connection of
    Unconnected _ ->
      Nothing

    Connected linked ->
      linked |> .link |> .userLink |> Just


collapse : Connection -> Connection
collapse = mapBasic Expandable.collapse


expand : Connection -> Connection
expand = mapBasic Expandable.expand


unwrap : Connection -> Basic
unwrap connection =
  case connection of
    Unconnected basic ->
      basic
    Connected linked ->
      linked.basic

mapBasic : (Basic -> Basic) -> Connection -> Connection
mapBasic basicFn connection =
  case connection of
    Unconnected basic ->
      Unconnected (basicFn basic)
    Connected linked ->
      Connected { linked | basic = basicFn linked.basic }


countLinked : List Connection -> Int
countLinked = List.length << List.filterMap userLink


either : (Basic -> a) -> (Linked -> a) -> Connection -> a
either basicFn linkedFn connection =
  case connection of
    Unconnected basic ->
      basicFn basic
    Connected linked ->
      linkedFn linked
