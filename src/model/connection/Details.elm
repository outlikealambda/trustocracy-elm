module Model.Connection.Details exposing
  ( Details
  )


import Common.Remote exposing (Remote)
import Model.Connection.Metrics exposing (Metrics)
import Model.Opinion.Opinion exposing (Opinion)


type alias Details =
  { opinion : Opinion
  , influence : Remote Int
  , metrics : Remote Metrics
  }
