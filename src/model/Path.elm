module Model.Path exposing
  ( Path
  , viewPaths
  , decoder
  , getLength
  )


import Model.Relationship as Relationship exposing (Relationship)
import Model.Trustee as Trustee exposing (Trustee)


import Json.Decode as Json exposing ((:=))
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class)


type alias Path =
  { trustee : Trustee
  , hops : List Relationship
  , score : Int
  }


decoder : Json.Decoder Path
decoder =
  Json.object3 Path
    ("trustee" := Trustee.decoder)
    ("hops" := Json.list Relationship.decoder)
    ("score" := Json.int)


viewPaths : List Path -> Html msg
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



viewHeader : Path -> Int -> Html msg
viewHeader {trustee, hops} count =
   div
    [ class "opg-header op single-line cf" ]
    [ div
      [ class "single-line path" ]
      [ Relationship.view trustee.relationship ]
    , div [class "op-text friend"] [ text trustee.name ]
    , div [class "single-line path"] (List.map Relationship.view hops)
    ]


viewAbbreviated : Path -> Html msg
viewAbbreviated {trustee, hops} =
  let relationships =
    List.map Relationship.view hops
  in
    div [class "op single-line cf"]
      [ div
        [ class "single-line path" ]
        [ Relationship.view trustee.relationship ]
      , div [class "op-text friend"] [text trustee.name]
      , div [class "single-line path"] relationships
      ]




getLength : Path -> Int
getLength = List.length << .hops
