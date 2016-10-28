module Model.Explorer exposing
  ( Explorer
  , Zoom
    (..)
  , empty
  , rotateSort
  , classifySort
  , sortConnections
  )


import Model.Connection.Connection as Connection exposing (Connection)
import Model.Question.Question exposing (Question)
import Model.Question.Assessor as Assessor exposing (Assessor)


import Dict exposing (Dict)


type alias Explorer =
  { connections : Dict OpinionId Connection
  , zoom : Zoom
  , questions : List Question
  , assessor : Assessor
  , sort : Sort
  }


type alias OpinionId = Int
type alias TopicId = Int


type Zoom
  = Focused OpinionId
  | Blurred


type Sort
  = Score
  | Descending
  | Ascending


empty : Explorer
empty =
  { connections = Dict.empty
  , zoom = Blurred
  , questions = []
  , assessor = Assessor.empty False
  , sort = Score
  }


rotateSort : Explorer -> Explorer
rotateSort explorer =
  { explorer
  | sort =
    case explorer.sort of
      Score ->
        Descending
      Descending ->
        Ascending
      Ascending ->
        Score
  }


classifySort : Sort -> String
classifySort sort =
  case sort of
    Score ->
      "default-sort"
    Ascending ->
      "ascending"
    Descending ->
      "descending"


sortConnections : Explorer -> List Connection
sortConnections { connections, sort } =
  let
    sorter =
      case sort of
        Score ->
          List.sortBy <| Connection.score
        Ascending ->
          List.sortBy <| Connection.influenceWithDefault 0
        Descending ->
          List.sortBy <| invert << Connection.influenceWithDefault 0

  in
    sorter <| Dict.values connections


invert : Int -> Int
invert =
  (-) 0
