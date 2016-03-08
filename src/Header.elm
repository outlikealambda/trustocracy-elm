module Header
  ( view
  , Context
  ) where


import ActiveUser exposing (ActiveUser (LoggedIn, LoggedOut))

import Html exposing (Html, Attribute, div, text, a)
import Html.Attributes exposing (class, href)
import Html.Events exposing (onClick, onWithOptions)
import Json.Decode as Json

import TransitRouter
import Routes


type alias Context =
  { logout : Signal.Address ()
  , login : Signal.Address ()
  }


view : Context -> ActiveUser -> Html
view context activeUser =
  let
    home =
      [ div
        [ class "home" ]
        [ a
          (clickTo <| Routes.Home)
          [ text "Home" ]
        ]
      ]

    (userName, login) =
      case activeUser of
        LoggedOut ->
          ( []
          , [ div [ class "login" ]
              [ a
                [ onClick context.login () ]
                [ text "login"]
              ]
            ]
          )
        LoggedIn user ->
          ( [ div
              [ class "user" ][ text user.name ] ]
          , [ div
              [ class "logout" ]
              [ a
                [ onClick context.logout () ]
                [ text "logout"]
              ]
            ]
          )

  in
    div [ class "header" ]
      <| []
      ++ home
      ++ login
      ++ userName


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
