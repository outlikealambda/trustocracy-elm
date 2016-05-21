module Opinion.Path exposing
  ( Path
  , viewPaths
  , decoder
  , getOpinerName
  , getOpinionId
  , getLength
  )


import Common.Relationship as Relationship exposing (Relationship)
import Trustee exposing (Trustee)


import Json.Decode as Json exposing ((:=))
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class)


type alias Path =
  { friend: Trustee
  , path: List Relationship
  , opiner: Trustee
  , opinionId: Int
  , score: Int
  }


decoder : Json.Decoder Path
decoder =
  Json.object5 Path
    ("friend" := Trustee.decoder)
    ("path" := Json.list Relationship.decoder)
    ("opiner" := Trustee.decoder)
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
viewHeader {friend, path} count =
   div
    [ class "opg-header op single-line cf" ]
    [ div
      [ class "single-line path" ]
      [ Relationship.view friend.relationship ]
    , div [class "op-text friend"] [ text friend.name ]
    , div [class "single-line path"] (List.map Relationship.view path)
    ]


viewAbbreviated : Path -> Html
viewAbbreviated {friend, path} =
  let relationships =
    List.map Relationship.view path
  in
    div [class "op single-line cf"]
      [ div
        [ class "single-line path" ]
        [ Relationship.view friend.relationship ]
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
