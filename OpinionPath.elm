module OpinionPath (Model, view, viewHeader, decoder, compareOP) where

import Json.Decode as Json exposing ((:=))
import Relationship
import User

import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class)

type alias Model = {
  friend: User.Model,
  path: List Relationship.Model,
  opiner: User.Model,
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
    div [class "single-line"]
      [ span [class "friend"] [text op.friend.name]
      , div [class "single-line path"] relationships
      , span [class "opiner"] [text op.opiner.name]
      , span [class "opinion-id"] [text <| toString op.opinionId]
      ]


viewHeader : Model -> Int -> Html
viewHeader op count =
   div [class "opg-header single-line"]
      [ div [class "op-text"] [ text op.friend.name ]
      , div [class "single-line path"] (List.map Relationship.view op.path)
      , div [class "path-count"] [text <| toString count]
      , div [class "op-text"] [ text op.opiner.name ]
      ]
