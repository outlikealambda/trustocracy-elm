module OpinionPathGroup
  ( Model
  , Action(SetOpinion)
  , init
  , view
  , update
  , fromOpinionPaths
  , opinionKeyGen
  ) where


import Opinion
import OpinionPath as OP
import Relationship


import Effects exposing (Effects)
import Task
import Html exposing (Html, Attribute, div, span, text, button)
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
  let sorted =
        List.sortBy .score ops
  in
        Model False sorted (Opinion.init opinionId)


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
      ( model
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
  let
    header =
      List.head opg.paths

    remainder =
      if opg.expanded then
        List.tail opg.paths |> Maybe.withDefault []
      else []

  in
    case header of

      Just h ->
        let
          opgHeader =
            OP.viewHeader h (List.length opg.paths)

          others =
            List.map OP.viewAbbreviated remainder

          opiner =
            OP.viewOpiner h

          clickAction =
            if opg.expanded then Collapse else Expand

          toggleClass =
            if opg.expanded then "expanded" else "collapsed"

        in
          div [class "opg col m12 l6"]
            [ div [class "t-card"]
              [ div [class "t-card-content"]
                  [ div
                    [ class ("t-card-title toggles " ++ toggleClass)
                    , onClick address clickAction ]
                    (opgHeader :: others)
                  , div [class "t-card-subtitle"] [opiner]
                  , div [class "t-card-body"]
                      [ Opinion.view opg.opinion
                      ]
                  ]
              ]
            ]

      Nothing -> div [] []


opinionKeyGen : (OP.Model -> Int)
opinionKeyGen = .opinionId
