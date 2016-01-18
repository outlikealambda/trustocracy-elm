module Relationship (Model, view) where

import Html exposing (Html, div)
import Html.Attributes exposing (class)

type alias Model = String

view : Model -> Html
view relationship =
  div [class (getCircleType relationship)] []

getCircleType : Model -> String
getCircleType relationship =
  case relationship of
    "TRUSTS_EXPLICITLY" -> "circle-explicit"
    "TRUSTS" -> "circle-regular"
    _ -> "circle-unknown"
