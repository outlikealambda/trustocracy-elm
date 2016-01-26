module Composer
  ( Model
  , init
  , Action
  , update
  , view
  ) where

import String
import Char
import Html exposing (Html, input, div, textarea, text, h3, button)
import Html.Attributes exposing (class, placeholder, value)
import Html.Events exposing (on, targetValue, keyCode)
import Effects exposing (Effects)
import Json.Decode as Json exposing ((:=))

import Markdown

type alias Model =
  { topicId: Int
  , text : String
  }

init : Int -> Model
init tid = Model tid ""

type Action
  = Save
  | Write String
  | Publish

update : Action -> Model -> (Model, Effects Action)
update message model =
  case message of
    Write rawText ->
      ( { model | text = rawText }
      , Effects.none
      )

    Save ->
      ( model, Effects.none )

    Publish ->
      ( model, Effects.none )


view : Signal.Address Action -> Model -> Html
view address model =
  div [ class "row composer" ]
    [ div [ class "col m12 l6" ]
      [ div [ class "t-card" ]
        [ div [ class "t-card-body" ]
          [ div [ class "subtitle" ] [ text "Write" ]
          , div [ class "input-field" ]
            [ textarea
              [ class "write materialize-textarea"
              , placeholder "Let's write something!"
              , value model.text
              , on "input" textDecoder (Signal.message address << Write)
              ]
              []
            ]
          , div [ class "character-count" ]
            [ String.length model.text
              |> toString
              |> flip (++) " characters written"
              |> text
            ]
          ]
        ]
      ]
    , div [ class "col m12 l6 preview" ]
      [ div [ class "t-card" ]
        [ div [ class "t-card-body" ]
          [ div [ class "subtitle" ] [ text "Preview" ]
          , div [ class "markdown" ] [ Markdown.toHtml model.text ]
          ]
        ]
      ]
    , div [ class "col s12" ]
      [ button [ class "publish forward-action"] [ text "publish" ] ]
    ]


textDecoder : Json.Decoder String
textDecoder = targetValue
