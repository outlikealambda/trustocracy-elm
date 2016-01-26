module OpinionPath
  ( Model
  , view
  , viewAbbreviated
  , viewHeader
  , viewOpiner
  , decoder
  , compareScore
  ) where

import Json.Decode as Json exposing ((:=))
import Relationship
import User exposing (User)

import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class)

type alias Model = {
  friend: User,
  path: List Relationship.Model,
  opiner: User,
  opinionId: Int,
  score: Int
}

decoder : Json.Decoder Model
decoder =
  Json.object5 Model
    ("friend" := User.decoder)
    ("path" := Json.list Json.string)
    ("opiner" := User.decoder)
    ("opinion" := Json.int)
    ("score" := Json.int)

compareOP : Model -> Model -> Order
compareOP a b =
  case compare a.opiner.name b.opiner.name of
    LT -> LT
    GT -> GT
    EQ -> compareScore a b

compareScore : Model -> Model -> Order
compareScore a b =
  case compare a.score b.score of
    LT -> LT
    GT -> GT
    EQ -> compare a.friend.name b.friend.name

view : Model -> Html
view op =
  let relationships =
    List.map Relationship.view op.path
  in
    div [class "op single-line"]
      [ span [class "op-text friend"] [text op.friend.name]
      , span [class "single-line path"] relationships
      ]

viewHeader : Model -> Int -> Html
viewHeader op count =
   div [class "opg-header op single-line cf"]
      [ span [class "op-text friend"] [ text op.friend.name ]
      , span [class "single-line path"] (List.map Relationship.view op.path)
      , span [class "path-count"] [text <| toString count]
      ]

viewAbbreviated : Model -> Html
viewAbbreviated op =
  let relationships =
    List.map Relationship.view op.path
  in
    div [class "op single-line cf"]
      [ span [class "op-text friend"] [text op.friend.name]
      , span [class "single-line path"] relationships
      ]

viewOpiner : Model -> Html
viewOpiner op =
  div [class "opiner subtitle"] [ text op.opiner.name ]
