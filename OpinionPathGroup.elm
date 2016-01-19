module OpinionPathGroup (Model, Action, init, view, update, opinerKeyGen, friendKeyGen, decoder) where

-- import Effects exposing (Effects, map, batch, Never)
import OpinionPath as OP
import Relationship

import Effects exposing (Effects)
import Html exposing (Html, div, span, text, button)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Json.Decode as Json exposing ((:=))

type alias Model =
  { expanded : Bool
  , paths : List OP.Model
  }


type Action
  = Expand
  | Collapse


init : (Model, Effects Action)
init =
  ( Model False []
  , Effects.none
  )

update : Action -> Model -> (Model, Effects Action)
update message model =
  case message of
    Expand ->
      ( { model | expanded = True }
      , Effects.none )
    Collapse ->
      ( { model | expanded = False }
      , Effects.none )


view : Signal.Address Action -> Model -> Html
view = viewByOpinion


viewByOpinion : Signal.Address Action -> Model -> Html
viewByOpinion address opg =
  let header =
        List.head opg.paths
      remainder =
        if opg.expanded then
          List.tail opg.paths |> Maybe.withDefault []
        else []

  in
    case header of

      Just h ->
        div [class "opg"]
          [ OP.viewHeader h (List.length opg.paths)
          , viewToggle address opg.expanded
          , div [class "opg-others"] (List.map OP.view remainder)
          ]

      Nothing -> div [] []


viewToggle : Signal.Address Action -> Bool -> Html
viewToggle address expanded =
  if expanded then
    button [ onClick address Collapse ] [ text "-" ]
  else
    button [ onClick address Expand ] [ text "+" ]


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
