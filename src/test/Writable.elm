module Test.Writable
  ( AsWritable
  , Action
  , update
  , view
  ) where


import Test.Opinion as Opinion exposing (Opinion)


import String
import Html exposing (Html, div, textarea, text)
import Html.Attributes exposing (class, placeholder, value)
import Html.Events exposing (on, targetValue)
import Effects exposing (Effects)


type alias AsWritable m =
  { m
  | opinion: Opinion
  }


type Action
  = Write String


update : Action -> AsWritable m -> (AsWritable m, Effects Action)
update action model =
  case action of
    Write raw ->
      ( { model | opinion = Opinion.setText model.opinion raw }
      , Effects.none
      )


view : Signal.Address Action -> AsWritable m -> Html
view address {opinion} =
  div [ class "opinion-creator" ]
    [ div [ class "input-field" ]
      [ textarea
        [ class "write"
        , placeholder "Let's write something!"
        , value opinion.text
        , on "input" targetValue (Signal.message address << Write)
        ]
        []
      ]
    , div [ class "character-count" ]
      [ String.length opinion.text
        |> toString
        |> flip (++) " characters written"
        |> text
      ]
    ]
