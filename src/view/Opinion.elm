module View.Opinion exposing
  ( view
  )


import Model.Opinion as Opinion exposing (Opinion)
import View.Author as AuthorView
import View.Qualifications as QualificationsView


import Html exposing (Html)
import Html.Attributes exposing (class)
import Markdown


view : Bool -> Opinion -> Html msg
view expanded {qualifications, text, snippet, id, author, influence} =
  let
    qualificationsHtml =
      Maybe.map QualificationsView.view qualifications
      |> Maybe.withDefault QualificationsView.noQualifications
  in
    case expanded of
      True ->
        Html.div
          [ class "opinion-full" ]
          [ AuthorView.view author influence
          , qualificationsHtml
          , Html.div
            [ class "text markdown" ]
            [ Markdown.toHtml [] text ]
          ]
      False ->
        Html.div
          [ class "opinion-snippet" ]
          [ AuthorView.view author influence
          , qualificationsHtml
          , Html.div
            [ class "text snippet" ]
            [ Html.p
              []
              [ Html.text snippet ]
            ]
        ]
