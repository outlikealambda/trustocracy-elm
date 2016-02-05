module Neighbor (Model, view, decoder) where

import User
import Relationship

import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class)
import Json.Decode as Json exposing((:=))


type alias Model = {
  user: User.Model,
  relationship: Relationship.Model
}


view : Model -> Html
view n =
  div [class "single-line"]
    [ Relationship.view n.relationship
    , span [] [text n.user.name]
    ]


decoder : Json.Decoder Model
decoder =
  Json.object2 Model
    ("friend" := User.decoder)
    ("rel" := Json.string)
