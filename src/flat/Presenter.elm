module Flat.Presenter
  ( Presenter
  , prepare
  , view
  , expand
  , collapse
  ) where


import Html exposing (Html, div, text, p)
import Html.Attributes exposing (class)
import Markdown
import String


type alias Presenter a =
  { a
  | text : String
  , snippet : String
  , expanded : Bool
  , fetched : Bool
  }


prepare : Presenter a -> Presenter a
prepare p = { p | snippet = snippetize 200 p.text }


expand : Presenter a -> Presenter a
expand presenter =
  { presenter | expanded = True }


collapse : Presenter a -> Presenter a
collapse presenter =
  { presenter | expanded = False }


view : Presenter a -> Html
view {text, expanded, snippet, fetched} =
  let
    v =
      if expanded then
        viewFull text
      else
        viewSnippet snippet

    presenterClasses =
      "opinion " ++
        if fetched then "fetched" else ""

  in
    div
      [ class presenterClasses ]
      [ v ]


viewFull : String -> Html
viewFull text =
  div [ class "text markdown"] [ Markdown.toHtml text ]


viewSnippet : String -> Html
viewSnippet snippet =
  div [ class "text snippet" ]
    [ p [] [ text snippet ]
    ]


snippetize : Int -> String -> String
snippetize maxLength s =
  -- add a space on the end to avoid removing the last word every time
  s ++ " "
    |> String.indexes " "
    |> maxValLessThan maxLength
    |> maybeSlice s


maxValLessThan : Int -> List Int -> Maybe Int
maxValLessThan maxVal ns =
  List.filter ((>) maxVal) ns
    |> List.maximum


maybeSlice : String -> Maybe Int -> String
maybeSlice s maybeBound =
  case maybeBound of
    Nothing -> s
    Just bound ->
      if bound < String.length s then
        (String.slice 0 bound s) ++ "..."
      else
        s
