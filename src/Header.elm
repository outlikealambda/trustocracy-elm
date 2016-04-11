module Header
  ( view
  , Context
  ) where


import Html exposing (Html, Attribute, div, text, a)
import Html.Attributes exposing (class, href)
import Html.Events exposing (onWithOptions)
import Json.Decode as Json

import TransitRouter
import Routes


type alias Context =
  { logout : Signal.Address ()
  , login : Signal.Address ()
  }


view : Html -> Html
view authHeader =
  let
    home =
      [ div
        [ class "home" ]
        [ a
          (clickTo <| Routes.Home)
          [ text "Home" ]
        ]
      ]

  in
    div [ class "header" ]
      <| home
      ++ [ authHeader ]


clickTo : Routes.Route -> List Attribute
clickTo route =
  let
    path =
      Routes.encode route
  in
    [ href path
    , onWithOptions
      "click"
      { stopPropagation = True, preventDefault = True }
      Json.value
      (\_ -> Signal.message TransitRouter.pushPathAddress path)
    ]
