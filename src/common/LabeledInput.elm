module Common.LabeledInput exposing
  ( view
  )

import Html exposing (Html, input, div, label, text)
import Html.Attributes exposing (class, value)
import Html.Events exposing (on, targetValue)

view : (String -> Signal.Message) -> (String, String) -> List Html
view address (tLabel, tValue) =
  [ label [] [ text tLabel ]
  , input
    [ value tValue
    , on "input" targetValue address
    ]
    []
  ]
