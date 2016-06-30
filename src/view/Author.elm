module View.Author exposing
  ( connection
  , withInfluence
  )


import Model.Trustee as Trustee exposing (Trustee)


import Html exposing (Html)
import Html.Attributes as Attrs exposing (class)


type alias Author = Trustee
type alias Influence = Int


connection : Author -> Int -> Html msg
connection author lineHeight =
  Html.div
    [ class "author"
    , Attrs.style [ ("line-height", toString lineHeight ++ "px") ]
    ]
    [ Html.div
      [ class "author-name" ]
      [ Html.text <| author.name ]
    ]


withInfluence : Author -> Influence -> Html msg
withInfluence {name} influence =
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
    ]
