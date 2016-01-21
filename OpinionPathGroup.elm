module OpinionPathGroup
  ( Model
  , Action(SetOpinion)
  , init
  , view
  , update
  , fromOpinionPaths
  , opinionKeyGen
  ) where

-- import Effects exposing (Effects, map, batch, Never)
import Opinion
import OpinionPath as OP
import Relationship

import Effects exposing (Effects)
import Task
import Html exposing (Html, div, span, text, button)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Json.Decode as Json exposing ((:=))


type alias Model =
  { expanded : Bool
  , paths : List OP.Model
  , opinion : Opinion.Model
  }


type Action
  = Expand
  | Collapse
  | SetOpinion (Int, String)
  | OpinionMsg Opinion.Action


fromOpinionPaths : Int -> List OP.Model -> Model
fromOpinionPaths opinionId ops =
  Model False ops (Opinion.init opinionId)


init : (Model, Effects Action)
init =
  ( Model False [] (Opinion.init 0)
  , Effects.none
  )


update : Action -> Model -> (Model, Effects Action)
update message model =
  case message of
    Expand ->
      ( { model | expanded = True }
      , OpinionMsg Opinion.Expand
          |> Task.succeed
          |> Effects.task
      )

    Collapse ->
      ( { model | expanded = False }
      , OpinionMsg Opinion.Collapse
          |> Task.succeed
          |> Effects.task
      )

    SetOpinion opinion ->
      ( Debug.log "opg set-opinion" model
      , OpinionMsg (Opinion.SetText <| snd opinion)
          |> Task.succeed
          |> Effects.task
      )

    OpinionMsg msg ->
      let (opinion, fx) =
        Opinion.update msg model.opinion
      in
        ( { model | opinion = opinion }
        , Effects.map OpinionMsg fx
        )


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
        let
            expandButton =
              viewToggle address opg.expanded

            opgHeader =
              OP.viewHeader h (List.length opg.paths) expandButton

            others =
              List.map OP.view remainder

        in
            div [class "opg cf"]
              [ opgHeader
              , div [class "others"] others
              , Opinion.view opg.opinion
              ]

      Nothing -> div [] []


viewToggle : Signal.Address Action -> Bool -> Html
viewToggle address expanded =
  if expanded then
    button [ onClick address Collapse, class "opg-toggle" ] [ text "-" ]
  else
    button [ onClick address Expand, class "opg-toggle" ] [ text "+" ]


opinionKeyGen : (OP.Model -> Int)
opinionKeyGen = .opinionId
