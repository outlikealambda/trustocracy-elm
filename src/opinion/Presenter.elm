module Opinion.Presenter
  ( Presenter
  , prepare
  , view
  , viewExpanded
  , viewCollapsed
  , expand
  , collapse
  ) where


import Html exposing (Html, div, text, p, span)
import Html.Attributes as Attribute exposing (class)
import Markdown
import String


import Trustee exposing (Trustee)
import Qualifications exposing (Qualifications)
import Routes


type alias Presenter a =
  { a
  | id : Int
  , text : String
  , influence : Int
  , opiner : Trustee
  , snippet : String
  , qualifications : Qualifications
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


viewExpanded : Presenter a -> Html
viewExpanded {text, qualifications, opiner, influence, fetched} =
  div
    [ class "opinion-full" ]
    [ Qualifications.view qualifications
    , div
      [ class "text markdown"]
      [ Markdown.toHtml text ]
    ]


viewCollapsed : (Int -> Routes.Route) -> Presenter a -> Html
viewCollapsed routeBuilder {id, snippet, opiner, influence, fetched} =
  div
    [ class "opinion-snippet" ]
    [ div
      [ class "text snippet" ]
      [ p [] [ Html.text snippet ] ]
    , span
      [ class "read-more"
      , Routes.goToRoute <| routeBuilder id
      ]
      [ Html.text "Read more..." ]
    ]

view : Bool -> (Int -> Routes.Route) -> Presenter a -> Html
view expanded routeBuilder presenter =
  div
    [ Attribute.classList
      [ ("opinion", True)
      , ("fetched", presenter.fetched)
      , ("expanded", expanded)
      , ("collapsed", not expanded)
      ]
    ]
    [ viewOpiner presenter.opiner presenter.influence
    , viewCollapsed routeBuilder presenter
    , viewExpanded presenter ]


plotClasses : Bool -> Bool -> String
plotClasses fetched expanded =
  "opinion " ++
    if fetched then "fetched" else ""


buildClasses : Bool -> String
buildClasses fetched =
  "opinion " ++
    if fetched then "fetched" else ""


viewSnippet : String -> Html
viewSnippet snippet =
  div [ class "text snippet" ]
    [ p [] [ text snippet ]
    ]

viewOpiner : Trustee -> Int -> Html
viewOpiner {name} influence =
  div
    [ class "opiner cf" ]
    [ div
      [ class "opiner-name" ]
      [ text <| name ]
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
