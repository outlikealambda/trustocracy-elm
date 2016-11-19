module Model.Explorer exposing
  ( Explorer
  , Zoom
    (..)
  , empty
  , rotateSort
  , classifySort
  , sortSurfacedOpinions
  )


import Model.SurfacedOpinion.SurfacedOpinion as SurfacedOpinion exposing (SurfacedOpinion)
import Model.Question.Question exposing (Question)
import Model.Question.Assessor as Assessor exposing (Assessor)


import Dict exposing (Dict)


type alias Explorer =
  { surfacedOpinions : Dict OpinionId SurfacedOpinion
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
  { surfacedOpinions = Dict.empty
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


sortSurfacedOpinions : Explorer -> List SurfacedOpinion
sortSurfacedOpinions { surfacedOpinions, sort } =
  let
    sorter =
      case sort of
        Score ->
          List.sortBy <| SurfacedOpinion.score
        Ascending ->
          List.sortBy <| SurfacedOpinion.influenceWithDefault 0
        Descending ->
          List.sortBy <| invert << SurfacedOpinion.influenceWithDefault 0

  in
    sorter <| Dict.values surfacedOpinions


invert : Int -> Int
invert =
  (-) 0
