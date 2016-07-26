module View.Place exposing (..)

import Model.Place as Place exposing (Place)

import Update.Place as PlaceUpdate

import Html.Events exposing (onClick, onInput)
import Html exposing (text, div, h1, h2, p, ul, li, body, Html, a, button, Attribute, input)
import Html.Attributes exposing (id, list, href, placeholder)

view : Place -> Html PlaceUpdate.Msg
view model =
  div
    [ id "locationInputs" ]
    [ h2
      []
      []
  , input
    [ placeholder "Location Name"
    , onInput PlaceUpdate.NameChange
    , Html.Attributes.value model.name
    ]
    []
  , input
    [ placeholder "Country Name"
    , onInput PlaceUpdate.CountryChange
    , Html.Attributes.value model.country
    ]
    []
  , input
      [ placeholder "City Name"
      , onInput PlaceUpdate.CityChange
      , Html.Attributes.value model.city
      ]
      []
  , input
      [ placeholder "Postal Number"
      , onInput PlaceUpdate.PostalChange
      , Html.Attributes.value model.postal
      ]
      []
  , div
      [ id "updateLocation" ]
      [ button
        [ onClick PlaceUpdate.UpdatePlace ]
        [ text "Update this location" ]
      ]
  , div
      [ id "deleteLocation" ]
      [ button
        [ onClick PlaceUpdate.RemovePlace ]
        [ text "Remove this location" ]
      ]
  ]
