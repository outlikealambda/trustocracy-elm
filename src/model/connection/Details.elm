module Model.Connection.Details exposing
  ( Details
  )


import Common.Remote exposing (Remote)
import Model.Extend.Expandable exposing (Expandable)
import Model.Opinion.Opinion exposing (Opinion)
import Model.Question.Assessor exposing (Assessor)


type alias Details =
  Expandable
    { opinion : Opinion
    , influence : Remote Int
    , assessor : Maybe Assessor
    }
