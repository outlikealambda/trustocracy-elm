module Opinion.Presenter exposing
  ( Presenter
  , prepare
  , view
  , viewExpanded
  , viewCollapsed
  , expand
  , collapse
  )


import Html exposing (Html, div, text, p, span)
import Html.Attributes as Attribute exposing (class)
import Markdown

import Model.Qualifications as Qualifications exposing (Qualifications)
import Model.Trustee exposing (Trustee)
import Routes
import Utils.String as StringUtils


type alias Presenter a =
  { a
  | id : Int
  , text : String
  , influence : Int
  -- , opiner : Trustee
  , snippet : String
  , qualifications : Qualifications
  , expanded : Bool
  , fetched : Bool
  }


prepare : Presenter a -> Presenter a
prepare p = { p | snippet = StringUtils.snippetize 200 p.text }


expand : Presenter a -> Presenter a
expand presenter =
  { presenter | expanded = True }


collapse : Presenter a -> Presenter a
collapse presenter =
  { presenter | expanded = False }


viewExpanded : Presenter a -> Html msg
viewExpanded {text, qualifications, influence, fetched} =
  div
    [ class "opinion-full" ]
    [ Qualifications.view qualifications
    , div
      [ class "text markdown"]
      [ Markdown.toHtml [] text ]
    ]


viewCollapsed : (Int -> msg) -> Presenter a -> Html msg
viewCollapsed buildRoute {id, snippet, influence, fetched} =
  div
    [ class "opinion-snippet" ]
    [ div
      [ class "text snippet" ]
      [ p [] [ Html.text snippet ] ]
    , span
      [ class "read-more"
      , Routes.goToRoute <| buildRoute id
      ]
      [ Html.text "Read more..." ]
    ]

view : Bool -> (Int -> msg) -> Presenter a -> Html msg
view expanded routeBuilder presenter =
  div
    [ Attribute.classList
      [ ("opinion", True)
      , ("fetched", presenter.fetched)
      , ("expanded", expanded)
      , ("collapsed", not expanded)
      ]
    ]
    -- [ viewOpiner presenter.opiner presenter.influence
    [ viewCollapsed routeBuilder presenter
    , viewExpanded presenter ]


plotClasses : Bool -> Bool -> String
plotClasses fetched expanded =
  "opinion " ++
    if fetched then "fetched" else ""


buildClasses : Bool -> String
buildClasses fetched =
  "opinion " ++
    if fetched then "fetched" else ""


viewSnippet : String -> Html msg
viewSnippet snippet =
  div [ class "text snippet" ]
    [ p [] [ text snippet ]
    ]

viewOpiner : Trustee -> Int -> Html msg
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
