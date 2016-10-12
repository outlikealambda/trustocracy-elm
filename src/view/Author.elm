module View.Author exposing
  ( connection
  -- , withInfluence
  )


import Model.Trustee as Trustee exposing (Trustee)
import Model.Relationship as Relationship


import Html exposing (Html)
import Html.Attributes as Attrs exposing (class)


type alias Author = Trustee
type alias Influence = Int


connection : Author -> Html msg
connection author =
  Html.div
    [ class "author" ]
    [ Html.div
      [ class <| "author-name " ++ Relationship.toClass author.relationship ]
      [ Html.text <| author.name ]
    ]
