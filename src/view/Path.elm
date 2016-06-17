module View.Path exposing
  (..)


import Model.Path as Path exposing (Path)
import Model.Relationship as Relationship


import Html exposing (Html)
import Html.Attributes exposing (class)


view : Path -> Html msg
view {trustee, hops} =
  Html.div
    []
    [ Html.div
      [ class <| Relationship.toClass trustee.relationship ]
      [ Html.text trustee.name ]
    , Html.div
      [ class "hops" ]
      (List.map Relationship.view hops)
    ]
