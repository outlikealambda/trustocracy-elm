module OpinionPath
  ( Model
  , view
  , viewAbbreviated
  , viewHeader
  , viewOpiner
  , decoder
  , getOpinionId
  ) where


import Relationship
import User exposing (User)


import Json.Decode as Json exposing ((:=))
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


getOpinionId : List Model -> Int
getOpinionId paths =
  case List.head paths of
    Nothing -> -1
    Just {opinionId} -> opinionId
