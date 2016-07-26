module View.Place exposing (..)

import Model.Place as Place exposing (Place)

import Update.Place as PlaceUpdate

import Html.Events exposing (onClick, onInput)
import Html exposing (text, div, h1, h2, p, ul, li, body, Html, a, button, Attribute, input)
import Html.Attributes exposing (id, list, href, placeholder, class)

view : Place -> Html PlaceUpdate.Msg
view model =
  div
    [ class "location-inputs" ]
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
      [ class "update-location" ]
      [ button
        [ onClick PlaceUpdate.UpdatePlace ]
        [ text "Update this location" ]
      ]
  , div
      [ class "delete-location" ]
      [ button
        [ onClick PlaceUpdate.RemovePlace ]
        [ text "Remove this location" ]
      ]
  ]
