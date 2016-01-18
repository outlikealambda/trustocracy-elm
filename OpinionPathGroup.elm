module OpinionPathGroup (Model) where

import Effects exposing (Effects, map, batch, Never)
import OpinionPath as OP
import Html exposing (Html, div)

type alias Model =
  { expanded : Boolean
  , paths : List OP.Model
  }

init : (Model, Effects Action)
init =
  ( Model False [],
  , Effects.none
  )

type Action
  = Expand
  | Collapse
