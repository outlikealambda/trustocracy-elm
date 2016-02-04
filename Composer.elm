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
import OpinionView as OView
import OpinionCreate as OCreate
import User exposing (User)
import Topic exposing (Topic)


type alias Model =
  { user : User
  , topic : Topic
  , opinion : OCreate.Model
  }


init : User -> Topic -> (Model, Effects Action)
init user topic =
  let
    ( opinion, fx ) =
      OCreate.init user.id topic
  in
    ( Model user topic opinion
    , Effects.map OCreateMsg fx
    )


type Action
  = OCreateMsg OCreate.Action
  | Publish


update : Action -> Model -> (Model, Effects Action)
update message model =
  case message of

    OCreateMsg msg ->
      let
        (updatedOpinion, fx) =
          OCreate.update msg model.opinion
      in
        ( { model | opinion = updatedOpinion }
        , Effects.map OCreateMsg fx
        )

    Publish ->
      ( model
      , Effects.none
      )


view : Signal.Address Action -> Model -> Html
view address model =
  div [ class "row composer" ]
    [ div [ class "col m12 l6" ]
      [ div [ class "t-card" ]
        [ div [ class "t-card-body" ]
          [ div [ class "subtitle" ] [ text "Write" ]
          , OCreate.viewCreator (Signal.forwardTo address OCreateMsg) model.opinion
          ]
        ]
      , div [ class "t-card" ]
        [ div [ class "t-card-body" ]
          [ OCreate.viewCredentialsInput (Signal.forwardTo address OCreateMsg) model.opinion ]
        ]
      ]
    , div [ class "col m12 l6 preview" ]
      [ div [ class "t-card" ]
        [ div [ class "t-card-body" ]
          [ div [ class "subtitle" ] [ text "Preview" ]
          , OView.view model.opinion
          ]
        ]
      , button [ class "publish forward-action"] [ text "publish" ]
      ]
    ]
