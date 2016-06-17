module Model.Explorer exposing
  ( Explorer
  , Zoom
    (..)
  , empty
  )


import Model.Connection as Connection exposing (Connection)


import Dict exposing (Dict)


type alias Explorer =
  { connections : Dict OpinionId Connection
  , zoom : Zoom
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
  }
