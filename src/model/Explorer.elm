module Model.Explorer exposing
  ( Explorer
  , Zoom
    (..)
  , empty
  )


import Model.Connection.Connection as Connection exposing (Connection)
import Model.Question.Question exposing (Question)


import Dict exposing (Dict)


type alias Explorer =
  { connections : Dict OpinionId Connection
  , zoom : Zoom
  , questions : List Question
  }


type alias OpinionId = Int
type alias TopicId = Int


type Zoom
  = Focused OpinionId
  | Blurred


empty : Explorer
empty =
  { connections = Dict.empty
  , zoom = Blurred
  , questions = []
  }
