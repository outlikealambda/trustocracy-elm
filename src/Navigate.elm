module Navigate
  ( view
  ) where


import User exposing (User)
import ActiveUser exposing (ActiveUser(LoggedOut))

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
            [ onClick ActiveUser.clear () ]
            [ text "Logout"]
          ]
        ]
  in
    div [ class "header" ]
      <| []
      ++ userBox
      ++ logoutBox
