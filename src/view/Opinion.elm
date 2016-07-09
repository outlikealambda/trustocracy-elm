module View.Opinion exposing
  ( text
  , kitchenSink
  )


import Model.Opinion.Record as Opinion
import Model.Qualifications as Qualifications
import View.Author as AuthorView
import View.Qualifications as QualificationsView


import Html exposing (Html)
import Html.Attributes exposing (class)
import Markdown


kitchenSink : Bool -> Opinion.Record a -> Html msg
kitchenSink expanded {qualifications, text, snippet, author, influence} =
  let
    nonEmpty =
      Qualifications.removeEmpty qualifications
    qualificationsHtml =
      if List.isEmpty nonEmpty then
        QualificationsView.noQualifications
      else
        QualificationsView.view nonEmpty
  in
    case expanded of
      True ->
        Html.div
          [ class "opinion-full" ]
          [ AuthorView.withInfluence author influence
          , qualificationsHtml
          , Html.div
            [ class "text markdown" ]
            [ Markdown.toHtml [] text ]
          ]
      False ->
        Html.div
          [ class "opinion-snippet" ]
          [ AuthorView.withInfluence author influence
          , qualificationsHtml
          , Html.div
            [ class "text snippet" ]
            [ Html.p
              []
              [ Html.text snippet ]
            ]
        ]


text : Bool -> Opinion.Record a -> Html msg
text expanded {text, snippet} =
  case expanded of
    True ->
      Html.div
        [ class "opinion-full" ]
        [ Html.div
          [ class "text markdown" ]
          [ Markdown.toHtml [] text ]
        ]
    False ->
        Html.div
          [ class "opinion-snippet" ]
          [ Html.div
            [ class "text snippet" ]
            [ Html.p
              []
              [ Html.text snippet ]
            ]
        ]
