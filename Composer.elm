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

import Credentials
import Opinion
import OpinionView
import User exposing (User)
import Topic exposing (Topic)

type alias Model = Opinion.Model


init : Int -> User -> Topic -> Model
init oid user topic =
  let model =
    Opinion.init oid user topic

  in
    { model | expanded = True }


type Action
  = Save
  | Write String
  | Publish
  | CredentialsMsg Credentials.Action


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

    CredentialsMsg msg ->
      ( { model | credentials = Credentials.update msg model.credentials}
      , Effects.none )


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
      , div [ class "t-card" ]
        [ div [ class "t-card-body" ]
          [ Credentials.viewForm (Signal.forwardTo address CredentialsMsg) model.credentials
          ]
        ]
      ]
    , div [ class "col m12 l6 preview" ]
      [ div [ class "t-card" ]
        [ div [ class "t-card-body" ]
          [ div [ class "subtitle" ] [ text "Preview" ]
          , OpinionView.view model
          ]
        ]
      , button [ class "publish forward-action"] [ text "publish" ]
      ]
    ]


textDecoder : Json.Decoder String
textDecoder = targetValue
