module Model.Connection exposing
  ( Connection
  , decoder
  , toDict
  , key
  , connectedCount
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


type alias Connection =
  Expandable
    { opinion : Opinion
    , influence : Remote Int
    , assessor : Maybe Assessor
    , score : Int
    , userLink : UserLink
    }


type alias UserLink = Maybe (List Path)


decoder : Decode.Decoder Connection
decoder =
  Decode.object2 fromApi
    ( "opinion" := Opinion.decoder )
    ( Decode.oneOf
      [ "paths" := Decode.map Just (Decode.list Path.decoder)
      , Decode.succeed Nothing
      ]
    )


fromApi : Opinion -> Maybe (List Path) -> Connection
fromApi opinion paths =
  { opinion = opinion
  , influence = Remote.requested 0
  , assessor = Nothing
  , userLink = Maybe.map sortPaths paths
  , score = Maybe.withDefault 1000 <| Maybe.map (minScore 0) paths
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
key = .opinion >> .id


connectedCount : List Connection -> Int
connectedCount =
  List.length << (List.filterMap .userLink)
