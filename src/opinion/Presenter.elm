module Opinion.Presenter
  ( Presenter
  , prepare
  , view
  , expand
  , collapse
  ) where


import Html exposing (Html, div, text, p, span)
import Html.Attributes exposing (class)
import Markdown
import String


import User exposing (User)


type alias Presenter a =
  { a
  | text : String
  , influence : Int
  , user : User
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
view {text, expanded, snippet, fetched, user, influence} =
  let
    opinionBody =
      if expanded then
        [ viewOpiner user influence
        , viewOpinion text
        ]
      else
        [ viewSnippet snippet ]

    presenterClasses =
      "opinion " ++
        if fetched then "fetched" else ""

  in
    div
      [ class presenterClasses ]
      opinionBody


viewOpinion : String -> Html
viewOpinion text =
  div [ class "text markdown"] [ Markdown.toHtml text ]


viewSnippet : String -> Html
viewSnippet snippet =
  div [ class "text snippet" ]
    [ p [] [ text snippet ]
    ]

viewOpiner : User -> Int -> Html
viewOpiner user influence =
  div
    [ class "opinion-header cf" ]
    [ div
      [ class "opiner" ]
      [ text <| user.name ]
    , div
      [ class "numbered-badge influence" ]
      [ span
        [ class "numbered-count" ]
        [ text <| toString influence ]
      , span
        [ class "numbered-label" ]
        [ text "influenced people" ]
      ]
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
