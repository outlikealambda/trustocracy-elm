module View.Path exposing (..)


import Model.Path as Path exposing (Path)


import Html exposing (Html)


view : Path -> Html msg
view {trustee, hops} =
  Html.div [] []
