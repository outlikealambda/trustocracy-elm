module Delegator
  ( view
  , navHeader
  ) where


import Html exposing (Html, div, text, p, span, a)
import Html.Attributes exposing (class)


import ActiveUser exposing (ActiveUser (LoggedIn, LoggedOut))
import Common.Relationship as Relationship
import Routes
import User exposing (User)
import Trustee exposing (Trustee)


type alias ViewContext =
  { user : User
  , updateUser : Signal.Address ActiveUser
  }


view : ViewContext -> Html
view {user, updateUser} =
  let
    bffs =
      List.filter (Trustee.isRelated Relationship.Bff) user.trustees
    trusted =
      List.filter (Trustee.isRelated Relationship.Trusted) user.trustees
  in
    div
      [ ]
      <| viewBffs bffs
      ++ viewTrusted trusted


viewBffs : List Trustee -> List Html
viewBffs bffs =
  bffs
  |> List.map (Html.text << .name)
  |> List.map (flip (::) [])
  |> List.map (div [ class "bff" ])


viewTrusted : List Trustee -> List Html
viewTrusted trusted =
  trusted
  |> List.map (Html.text << .name)
  |> List.map (flip (::) [])
  |> List.map (div [ class "trusted" ])


navHeader : ActiveUser -> List Html
navHeader activeUser =
  case activeUser of
    LoggedOut ->
      []

    LoggedIn user ->
      [ div
        [class "home" ]
        [ a
          (Routes.clickTo Routes.UserDelegates)
          [ text <| "You trust " ++ (toString <| List.length user.trustees) ++ " people" ]
        ]
      ]
