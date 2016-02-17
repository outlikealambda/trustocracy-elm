module Opinion.Credentials
  ( Model
  , init
  , Action
  , update
  , view
  , viewForm
  ) where

import String
import Html exposing (Html, input, div, label, text)
import Html.Attributes exposing (class, value)

import Common.LabeledInput as LabeledInput

type alias Model =
  { work: String
  , personal: String
  }

init : Model
init =
  Model "" ""

type Action
  = WriteWork String
  | WritePersonal String



update : Action -> Model -> Model
update message model =
  case message of
    WriteWork rawWork ->
      { model | work = rawWork }
    WritePersonal rawPersonal ->
      { model | personal = rawPersonal }

viewForm : Signal.Address Action -> Model -> Html
viewForm address model =
  let
      workForm =
        LabeledInput.view
          ( Signal.message address << WriteWork )
          ( "Do you have work experience with this issue?"
          , model.work )
      personalForm =
        LabeledInput.view
          ( Signal.message address << WritePersonal )
          ( "Do you have personal experience with this issue?"
          , model.personal )
  in List.append workForm personalForm
    |> div [ class "credentials-form" ]

view : Model -> Html
view model =
  if String.isEmpty model.work && String.isEmpty model.personal then
    div [ class "no-credentials" ] []
  else
    div [ class "credentials" ]
      [ div [ class "credentials-header"] [ text "the [user] testifies that:" ]
      , div [] [ text model.work ]
      , div [] [ text model.personal ]
      ]

    -- [ label [] [ text "Do you have work experience with this issue?" ]
    -- , input
    --   [ class "work-input"
    --   , value model.work
    --   , on "input" targetValue (Signal.message address << WriteWork)
    --   ]
    --   []
    -- , label [] [ text "Do you have personal experience with this issue?" ]
    -- , input
    --   [ class "personal-input"
    --   , value model.personal
    --   , on "input" targetValue (Signal.message address << WritePersonal)
    --    ]
    --   []
    -- ]
