module Model.Connection exposing
  ( Connection
  , decoder
  , toDict
  , key
  )


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
    , paths : List Path
    , score : Int
    , assessor : Assessor
    }


decoder : Decode.Decoder Connection
decoder =
  Decode.object2 fromApi
    ("opinion" := Opinion.decoder)
    ("paths" := Decode.list Path.decoder)


fromApi : Opinion -> List Path -> Connection
fromApi opinion paths =
  { opinion = opinion
  , paths = sortPaths paths
  , score = minScore paths
  , inflation = Expandable.Collapsed
  , assessor = Assessor.empty
  }


sortPaths : List Path -> List Path
sortPaths = List.sortBy .score


minScore : List Path -> Int
minScore = Maybe.withDefault 1000 << List.minimum << List.map .score


toDict : List Connection -> Dict Int Connection
toDict connections =
  Dict.fromList <| List.map keyPair connections


keyPair : Connection -> (Int, Connection)
keyPair c =
  (key c, c)


key : Connection -> Int
key = .opinion >> .id
