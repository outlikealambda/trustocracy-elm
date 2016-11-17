module View.Author exposing
  ( connected
  )


import Model.Trustee as Trustee exposing (Trustee)
import Model.Relationship as Relationship


import Html exposing (Html)
import Html.Attributes as Attrs exposing (class)


type alias Author = Trustee
type alias Influence = Int


connected : Author -> Html msg
connected author =
  Html.div
    [ class "author" ]
    [ Html.div
      [ class <| "author-name " ++ Relationship.toClass author.relationship ]
      [ Html.text <| author.name ]
    ]
