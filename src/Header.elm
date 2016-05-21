module Header exposing
  ( view
  )


import Html exposing (Html, Attribute, div, text, a)
import Html.Attributes exposing (class, href)

import Routes


view : (Routes.Route -> m) -> List (Html m) -> Html m
view onRoute sessionElements =
  let
    home =
      [ div
        [ class "home" ]
        [ a
          (Routes.clickTo onRoute Routes.Home)
          [ text "Home" ]
        ]
      ]

  in
    div [ class "header" ]
      <| home
      ++ sessionElements
