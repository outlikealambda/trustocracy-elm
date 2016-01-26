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
import OpinionCreate as Create
import User exposing (User)
import Topic exposing (Topic)


type alias Model =
  { user : User
  , topic : Topic
  , opinion : Opinion.Model
  }


init : User -> Topic -> Int -> Model
init user topic oid =
  Model user topic (Opinion.initExpanded oid)


type Action
  = CreateMsg Create.Action


update : Action -> Model -> (Model, Effects Action)
update message model =
  case message of
    CreateMsg msg ->
      let
          (updatedOpinion, fx) =
            Create.update msg model.opinion
      in
          ( { model | opinion = updatedOpinion }
          , Effects.map CreateMsg fx )


view : Signal.Address Action -> Model -> Html
view address model =
  div [ class "row composer" ]
    [ div [ class "col m12 l6" ]
      [ div [ class "t-card" ]
        [ div [ class "t-card-body" ]
          [ div [ class "subtitle" ] [ text "Write" ]
          , Create.viewCreator (Signal.forwardTo address CreateMsg) model.opinion
          ]
        ]
      , div [ class "t-card" ]
        [ div [ class "t-card-body" ]
          [ Create.viewCredentialsInput (Signal.forwardTo address CreateMsg) model.opinion ]
        ]
      ]
    , div [ class "col m12 l6 preview" ]
      [ div [ class "t-card" ]
        [ div [ class "t-card-body" ]
          [ div [ class "subtitle" ] [ text "Preview" ]
          , OpinionView.view model.opinion
          ]
        ]
      , button [ class "publish forward-action"] [ text "publish" ]
      ]
    ]
