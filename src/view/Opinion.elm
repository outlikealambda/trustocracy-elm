module View.Opinion exposing
  ( view
  )


import Model.Opinion as Opinion exposing (Opinion)
import View.Qualifications as QualificationsView


import Html exposing (Html)
import Html.Attributes exposing (class)


import Markdown


view : Bool -> Opinion -> Html msg
view expanded {qualifications, text, snippet, id} =
  case expanded of
    True ->
      Html.div
        [ class "opinion-full" ]
        [ QualificationsView.view qualifications
        , Html.div
          [ class "text markdown"]
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
