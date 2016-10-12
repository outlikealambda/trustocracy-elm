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
kitchenSink expanded {qualifications, text, preview, author} =
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
          [ AuthorView.connection author
          , qualificationsHtml
          , viewFull text
          ]
      False ->
        Html.div
          [ class "opinion-snippet" ]
          [ AuthorView.connection author
          , qualificationsHtml
          , viewPreview preview
        ]


text : Bool -> Opinion.Record a -> Html msg
text expanded {text, snippet} =
  case expanded of
    True ->
      Html.div
        [ class "opinion-full" ]
        [ viewFull text ]
    False ->
      Html.div
        [ class "opinion-snippet" ]
        [ viewSnippet snippet ]


viewSnippet : String -> Html msg
viewSnippet prose =
  Html.div
    [ class "text snippet" ]
    [ Html.p
      []
      [ Html.text prose ]
    ]


viewPreview : String -> Html msg
viewPreview prose =
  Html.div
    [ class "text preview" ]
    [ Html.p
      []
      [ Html.text prose ]
    ]


viewFull : String -> Html msg
viewFull prose =
  Html.div
    [ class "text markdown" ]
    [ Markdown.toHtml [] prose ]
