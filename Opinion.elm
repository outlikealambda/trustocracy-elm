module Opinion (Model, init, Action(Expand, Collapse, SetText), update, view) where

import String
import Effects exposing (Effects)

import Html exposing (Html, div, text, p)
import Html.Attributes exposing (class)

type alias Model =
  { id: Int
  , expanded: Bool
  , text : String
  , snippet : String
  }

init : Int -> Model
init oid =
  Model oid False "" ""


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
        | text = Debug.log "set-text" fullText
        , snippet = snippetize 200 fullText
        }
      , Effects.none )


view : Model -> Html
view model =
  let p =
        if model.expanded then
          viewFull model
        else
          viewSnippet model
  in
      div [ class "opinion" ] [ p ]


viewFull : Model -> Html
viewFull model =
  p [class "full-text"] [ text model.text ]


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
