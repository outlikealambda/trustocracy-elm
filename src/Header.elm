module Header exposing
  ( view
  , Context
  )


import Html exposing (Html, Attribute, div, text, a)
import Html.Attributes exposing (class, href)

import Routes


type alias Context =
  { logout : Signal.Address ()
  , login : Signal.Address ()
  }


view : List (Html m) -> Html m
view sessionElements =
  let
    home =
      [ div
        [ class "home" ]
        [ a
          (Routes.clickTo Routes.Home)
          [ text "Home" ]
        ]
      ]

  in
    div [ class "header" ]
      <| home
      ++ sessionElements
