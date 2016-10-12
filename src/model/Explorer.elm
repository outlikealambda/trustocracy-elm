module Model.Explorer exposing
  ( Explorer
  , Zoom
    (..)
  , empty
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
  }


type alias OpinionId = Int
type alias TopicId = Int


type Zoom
  = Focused OpinionId
  | Blurred


mapZoom : (Int -> a) -> Zoom -> Maybe a
mapZoom transform zoom =
  case zoom of
    Blurred ->
      Nothing
    Focused oid ->
      Just <| transform oid


empty : Explorer
empty =
  { connections = Dict.empty
  , zoom = Blurred
  , questions = []
  , assessor = Assessor.empty False
  }
