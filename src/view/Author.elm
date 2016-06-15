module View.Author exposing
  (view)


import Model.Trustee as Trustee exposing (Trustee)
import Model.Qualifications as Qualifications exposing (Qualifications)
import View.Qualifications as QualificationsView


import Html exposing (Html)
import Html.Attributes exposing (class)


type alias Author = Trustee
type alias Influence = Int


view : Author -> Influence -> Qualifications -> Html msg
view {name} influence qualifications =
  Html.div
    [ class "author cf" ]
    [ Html.div
      [ class "author-name" ]
      [ Html.text <| name ]
    , Html.div
      [ class "numbered-badge influence" ]
      [ Html.span
        [ class "numbered-count" ]
        [ Html.text <| toString influence ]
      , Html.span
        [ class "numbered-label" ]
        [ Html.text "influenced people" ]
      ]
    , QualificationsView.view qualifications
    ]
