module Opinion.View
  ( Model
  , init
  , Action(Expand, Collapse, SetText)
  , view
  , update
  ) where

import Effects exposing (Effects)
import Html exposing (Html, div, text, p)
import Html.Attributes exposing (class)
import Markdown

import Opinion.Model as Opinion
import Opinion.Credentials as Credentials


type alias Model = Opinion.Model


init : Int -> (Model, Effects Action)
init oid =
  ( Opinion.empty
  , Opinion.fetch oid
    |> Effects.map Init
  )


type Action
  = Init Model
  | Expand
  | Collapse
  | SetText String


update : Action -> Model -> (Model, Effects Action)
update message model =
  case message of
    Init retrieved ->
      ( retrieved
      , Effects.none )

    Expand ->
      ( { model | expanded = True }
      , Effects.none )

    Collapse ->
      ( { model | expanded = False }
      , Effects.none )

    SetText fullText ->
      ( Opinion.setText fullText model
      , Effects.none )


view : Model -> Html
view model =
  let v =
      if model.expanded then
        viewFull model
      else
        viewSnippet model
      creds =
        Credentials.view model.credentials
  in
      div [ class "opinion" ]
        [ creds
        , v
        ]


viewFull : Model -> Html
viewFull model =
  div [ class "text markdown"] [ Markdown.toHtml model.text ]


viewSnippet : Model -> Html
viewSnippet model =
  div [ class "text snippet" ]
    [ p [] [ text model.snippet ]
    ]
