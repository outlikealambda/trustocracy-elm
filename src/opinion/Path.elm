module Opinion.Path
  ( Model
  , viewAbbreviated
  , viewHeader
  , decoder
  , getOpinerName
  , getOpinionId
  , getLength
  ) where


import Common.Relationship as Relationship
import User exposing (User)


import Json.Decode as Json exposing ((:=))
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class)


type alias Model =
  { friendRelationship: Relationship.Model
  , friend: User
  , path: List Relationship.Model
  , opiner: User
  , opinionId: Int
  , score: Int
  }


decoder : Json.Decoder Model
decoder =
  Json.object6 Model
    ("friendRelationship" := Json.string)
    ("friend" := User.decoder)
    ("path" := Json.list Json.string)
    ("opiner" := User.decoder)
    ("opinion" := Json.int)
    ("score" := Json.int)


viewHeader : Model -> Int -> Html
viewHeader {friendRelationship, friend, path} count =
   div
    [ class "opg-header op single-line cf" ]
    [ div
      [ class "single-line path" ]
      [ Relationship.view friendRelationship ]
    , div [class "op-text friend"] [ text friend.name ]
    , div [class "single-line path"] (List.map Relationship.view path)
    , div
      [ class "path-count numbered-badge" ]
      [ span
        [ class "numbered-count" ]
        [ text <| toString count ]
      , span
        [ class "numbered-label" ]
        [ text "other connections" ]
      ]
    ]


viewAbbreviated : Model -> Html
viewAbbreviated {friendRelationship, friend, path} =
  let relationships =
    List.map Relationship.view path
  in
    div [class "op single-line cf"]
      [ div
        [ class "single-line path" ]
        [ Relationship.view friendRelationship ]
      , div [class "op-text friend"] [text friend.name]
      , div [class "single-line path"] relationships
      ]


getOpinerName : Model -> String
getOpinerName = .opiner >> .name


getOpinionId : List Model -> Int
getOpinionId paths =
  case List.head paths of
    Nothing -> -1
    Just {opinionId} -> opinionId


getLength : Model -> Int
getLength = List.length << .path
