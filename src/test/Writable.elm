module Test.Writable
  ( AsWritable
  , Action
  , OnUpdate
  , update
  , view
  ) where


import Test.Opinion as Opinion exposing (Opinion)


import String
import Html exposing (Html, div, textarea, text)
import Html.Attributes exposing (class, placeholder, value)
import Html.Events exposing (on, targetValue)
import Effects exposing (Effects)


type alias AsWritable m = m


type Action
  = Write String


type alias OnUpdate m =
  { write : AsWritable m -> String -> AsWritable m
  }


update : Action -> OnUpdate m -> AsWritable m -> (AsWritable m, Effects Action)
update action onUpdate model =
  case action of
    Write raw ->
      ( onUpdate.write model raw
      , Effects.none
      )


view : Signal.Address Action -> (m -> String) -> AsWritable m -> Html
view address f model =
  div [ class "opinion-creator" ]
    [ div [ class "input-field" ]
      [ textarea
        [ class "write"
        , placeholder "Let's write something!"
        , value (f model)
        , on "input" targetValue (Signal.message address << Write)
        ]
        []
      ]
    , div [ class "character-count" ]
      [ String.length (f model)
        |> toString
        |> flip (++) " characters written"
        |> text
      ]
    ]
