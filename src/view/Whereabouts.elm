module View.Whereabouts exposing (..)

import Model.Whereabouts as Whereabouts exposing (Whereabouts)
import Model.Place as Place exposing (Place)

import Update.Whereabouts as WhereaboutsUpdate

import View.Place as PlaceView

import Html.App
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (id, list, href, placeholder)
import Html exposing (text, div, h1, h2, p, ul, li, body, Html, a, button, Attribute, input)

view : Whereabouts -> Html WhereaboutsUpdate.Msg
view model =
  div
    [ id "header" ]
    [ h2
      []
      [ text "edit user (location) information" ]
  , div
      [ id "addLocationFields" ]
      [ text "Constituent Of: " ]
  , div
      [ id "currentLocations" ]
      (List.map viewCurrentLocations model)
  , div
      [ id "addLocation" ]
      [ button
        [ onClick WhereaboutsUpdate.AddPlace ]
        [ text "Add new location" ]
      ]
  ]
viewCurrentLocations: Place -> Html WhereaboutsUpdate.Msg
viewCurrentLocations place =
  Html.App.map ( WhereaboutsUpdate.Modify place.id ) (PlaceView.view place)
