module View.Whereabouts exposing (..)

import Model.Whereabouts as Whereabouts exposing (Whereabouts)
import Model.Place as Place exposing (Place)

import Update.Whereabouts as WhereaboutsUpdate

import View.Place as PlaceView

import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (id, list, href, placeholder, class)
import Html exposing (text, div, h1, h2, p, ul, li, body, Html, a, button, Attribute, input)

view : Whereabouts -> Html WhereaboutsUpdate.Msg
view model =
  div
    []
    [ h2
      []
      [ text "edit user (location) information" ]
  , div
      [ class "location-fields" ]
      [ text "Constituent Of: " ]
  , div
      [ class "current-locations" ]
      (List.map viewCurrentLocations model)
  , div
      [ class "add-location" ]
      [ button
        [ onClick WhereaboutsUpdate.AddPlace ]
        [ text "Add new location" ]
      ]
  ]
viewCurrentLocations: Place -> Html WhereaboutsUpdate.Msg
viewCurrentLocations place =
  Html.map ( WhereaboutsUpdate.Modify place.id ) (PlaceView.view place)
