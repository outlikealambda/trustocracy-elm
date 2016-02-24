module Flat.Writer
  ( Writer
  , Action
  , update
  , view
  ) where


import String
import Html exposing (Html, div, textarea, text)
import Html.Attributes exposing (class, placeholder, value)
import Html.Events exposing (on, targetValue)


type alias Writer a =
  { a
  | text : String
  }


type Action
  = Write String


update : Action -> Writer a -> Writer a
update action writer =
  case action of
    Write raw ->
      { writer | text = raw }


view : Signal.Address Action -> Writer a -> Html
view address writer =
  div [ class "opinion-creator" ]
    [ div [ class "input-field" ]
      [ textarea
        [ class "write"
        , placeholder "Let's write something!"
        , value writer.text
        , on "input" targetValue (Signal.message address << Write)
        ]
        []
      ]
    , div [ class "character-count" ]
      [ String.length writer.text
        |> toString
        |> flip (++) " characters written"
        |> text
      ]
    ]
