module Header
  ( view
  ) where


import User exposing (User)


import String
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)


view : User -> Html
view user =
  let
    userBox =
      if String.isEmpty (Debug.log "header user" user.name) then
        []
      else
        [ div [ class "user" ][ text <| user.name ] ]
  in
    div [ class "header" ]
      <| []
      ++ userBox
