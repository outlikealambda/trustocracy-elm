module Header
  ( view
  ) where


import User exposing (User)
import LocalStorage

import String
import Html exposing (Html, div, text, button)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)


view : User -> Html
view user =
  let
    userBox =
      if String.isEmpty user.name then
        []
      else
        [ div [ class "user" ][ text user.name ] ]

    logoutBox =
      if String.isEmpty user.name then
        []
      else
        [ div [ class "user" ]
          [ button
            [ onClick LocalStorage.clearActiveUserAddress () ]
            [ text "Logout"]
          ]
        ]
  in
    div [ class "header" ]
      <| []
      ++ userBox
      ++ logoutBox
