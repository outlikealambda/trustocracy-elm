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
import Auth exposing (Auth)
import ActiveUser exposing (ActiveUser(LoggedIn, LoggedOut))


type alias Context =
  { logout : Signal.Address ()
  , login : Signal.Address ()
  }


view : Html -> Auth -> Html
view authHeader auth =
  let
    home =
      [ div
        [ class "home" ]
        [ a
          (clickTo Routes.Home)
          [ text "Home" ]
        ]
      ]

    youtrust = 
      case auth.activeUser of
        LoggedOut ->
          []

        LoggedIn user ->
          [ div
            [class "home" ]
            [ a
              (clickTo Routes.UserDelegates)
              [ text <| "You trust " ++ (toString <| List.length user.trustees) ++ " people" ]
            ]
          ]
  in
    div [ class "header" ]
      <| home
      ++ youtrust
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
