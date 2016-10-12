module Model.Connection.Connection exposing
  ( Connection
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
  )


import Common.Extended as Extended exposing (Extended)
import Common.Remote as Remote exposing (Remote)


import Model.Connection.Details exposing (Details)
import Model.Connection.Link exposing (Link, UserLink)
import Model.Extend.Expandable as Expandable
import Model.Opinion.Opinion as Opinion exposing (Opinion)
import Model.Path as Path
import Model.Question.Assessor exposing (Assessor)


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
  Decode.object1 (Extended.Basic << basicFromApi) Opinion.decoder


connectedFromApi : UserLink -> Opinion -> Connection
connectedFromApi paths opinion =
  Extended.Complex
    (basicFromApi opinion)
    (linkFromApi paths)


linkFromApi : UserLink -> Link
linkFromApi userLink =
  { userLink = sortUserLink userLink
  , score = minScore 0 userLink
  }


basicFromApi : Opinion -> Details
basicFromApi opinion =
  { opinion = opinion
  , influence = Remote.NoRequest
  , assessor = Nothing
  , inflation = Expandable.Collapsed
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


setInfluence : Remote Int -> Connection -> Connection
setInfluence influence =
  mapDetails (\b -> { b | influence = influence})


assessor : Connection -> Maybe Assessor
assessor = .assessor << unwrap


setAssessor : Maybe Assessor -> Connection -> Connection
setAssessor assessor =
  mapDetails (\b -> { b | assessor = assessor})


inflation : Connection -> Expandable.Inflation
inflation = .inflation << unwrap


userLink : Connection -> Maybe UserLink
userLink connection =
  case connection of
    Extended.Basic _ ->
      Nothing

    Extended.Complex _ link ->
      link |> .userLink |> Just


collapse : Connection -> Connection
collapse = mapDetails Expandable.collapse


expand : Connection -> Connection
expand = mapDetails Expandable.expand


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
