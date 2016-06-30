module Model.Connection exposing
  ( Connection
  , decoder
  )


import Model.Expandable as Expandable exposing (Expandable)
import Model.Opinion as Opinion exposing (Opinion)
import Model.Path as Path exposing (Path)


import Json.Decode as Decode exposing ((:=))


type alias Connection =
  Expandable
    { opinion : Opinion
    , paths : List Path
    , score : Int
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
  , status = Expandable.Collapsed
  }


sortPaths : List Path -> List Path
sortPaths = List.sortBy .score


minScore : List Path -> Int
minScore = Maybe.withDefault 1000 << List.minimum << List.map .score
