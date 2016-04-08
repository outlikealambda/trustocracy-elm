module Opinion.Path
  ( Path
  , viewPaths
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


type alias Path =
  { friendRelationship: Relationship.Model
  , friend: User
  , path: List Relationship.Model
  , opiner: User
  , opinionId: Int
  , score: Int
  }


decoder : Json.Decoder Path
decoder =
  Json.object6 Path
    ("friendRelationship" := Json.string)
    ("friend" := User.decoder)
    ("path" := Json.list Json.string)
    ("opiner" := User.decoder)
    ("opinion" := Json.int)
    ("score" := Json.int)


viewPaths : List Path -> Html
viewPaths paths =
  case paths of
    first::rest ->
      div
        [ class "paths" ]
        <| viewHeader first (List.length paths)
        :: List.map viewAbbreviated rest
    [] ->
      div
        [ class "paths" ]
        [ text "too far to calculate path" ]



viewHeader : Path -> Int -> Html
viewHeader {friendRelationship, friend, path} count =
   div
    [ class "opg-header op single-line cf" ]
    [ div
      [ class "single-line path" ]
      [ Relationship.view friendRelationship ]
    , div [class "op-text friend"] [ text friend.name ]
    , div [class "single-line path"] (List.map Relationship.view path)
    ]


viewAbbreviated : Path -> Html
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


getOpinerName : Path -> String
getOpinerName = .opiner >> .name


getOpinionId : List Path -> Int
getOpinionId paths =
  case List.head paths of
    Nothing -> -1
    Just {opinionId} -> opinionId


getLength : Path -> Int
getLength = List.length << .path
