module Delegator
  ( Delegator
  , Action(..)
  , empty
  , update
  , view
  ) where


import Effects exposing (Effects)
import Html exposing (Html, div, text, p, span)


type alias Delegator =
  { foo : String
  }


empty : Delegator
empty =
  { foo = "" }


type alias UserId = Int


type Action
  = Add UserId
  | Remove UserId
  | NoOp


update : Action -> Delegator -> (Delegator, Effects Action)
update action model =
  (model, Effects.none)


view : Signal.Address Action -> Delegator -> Html
view address model =
  div [] [ text "hi" ]
