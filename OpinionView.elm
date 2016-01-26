module OpinionView
  ( Model
  , init
  , Action(Expand, Collapse, SetText)
  , view
  , update
  ) where

import String
import Effects exposing (Effects)
import Html exposing (Html, div, text, p)
import Html.Attributes exposing (class)
import Markdown

import Opinion
import Credentials


type alias Model = Opinion.Model


init = Opinion.init


type Action
  = Expand
  | Collapse
  | SetText String


update : Action -> Model -> (Model, Effects Action)
update message model =
  case message of

    Expand ->
      ( { model | expanded = True }
      , Effects.none )

    Collapse ->
      ( { model | expanded = False }
      , Effects.none )

    SetText fullText ->
      ( { model
        | text = fullText
        , snippet = snippetize 132 fullText
        }
      , Effects.none )


view : Model -> Html
view model =
  let p =
        viewFull model
      creds =
        Credentials.view model.credentials
  in
      div [ class "opinion" ]
        [ creds
        , p
        ]


viewFull : Model -> Html
viewFull model =
  div [class "text"] [ Markdown.toHtml model.text ]


viewSnippet : Model -> Html
viewSnippet model =
  p [ class "snippet" ] [ text model.snippet ]


snippetize : Int -> String -> String
snippetize maxLength s =
  -- add a space on the end to avoid removing the last word every time
  s ++ " "
    |> String.indexes " "
    |> maxValLessThan maxLength
    |> maybeSlice s


maybeSlice : String -> Maybe Int -> String
maybeSlice s maybeBound =
  case maybeBound of
    Nothing -> s
    Just bound ->
      if bound < String.length s then
        (String.slice 0 bound s) ++ "..."
      else
        s


maxValLessThan : Int -> List Int -> Maybe Int
maxValLessThan maxVal ns =
  List.filter ((>) maxVal) ns
    |> List.maximum
