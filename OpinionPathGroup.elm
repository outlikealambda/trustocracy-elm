module OpinionPathGroup (Model, view, opinerKeyGen, friendKeyGen, decoder) where

-- import Effects exposing (Effects, map, batch, Never)
import OpinionPath as OP
import Relationship

import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class)
import Json.Decode as Json exposing ((:=))

type alias Model =
  { expanded : Bool
  , paths : List OP.Model
  }

-- init : (Model, Effects Action)
-- init =
--   ( Model False [],
--   , Effects.none
--   )

-- type Action
--   = Expand
--   | Collapse

viewByOpinion : Model -> Html
viewByOpinion opg =
  let header =
        List.head opg.paths
      remainder =
        List.tail opg.paths
  in
    case header of
      Just h ->
        div [class "opg-header"]
          [ span [] [ text h.friend.name ]
          , div [] (List.map Relationship.view h.path)
          , span [class "path-count"] [text <| toString <| List.length h.path]
          , span [] [ text h.opiner.name ]
          ]
      Nothing -> div [] []

view : Model -> Html
view = viewByOpinion

opinerKeyGen : (OP.Model -> String)
opinerKeyGen = .opiner >> .name

friendKeyGen : (OP.Model -> String)
friendKeyGen = .friend >> .name

decoder : Json.Decoder Model
decoder =
  let asOP =
    "paths" := Json.list OP.decoder
  in
    Json.map (Model False) asOP
