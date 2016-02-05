module OpinionPathGroup
  ( Model
  , Action
  , initGroups
  , view
  , update
  , toDict
  ) where


import OpinionView as OView
import OpinionPath as OPath
import Relationship


import Effects exposing (Effects)
import Task
import Html exposing (Html, Attribute, div, span, text, button)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Json.Decode as Json exposing ((:=))
import Dict


type alias Model =
  { groupId : Int
  , expanded : Bool
  , paths : List OPath.Model
  , opinion : OView.Model
  }


type Action
  = Expand
  | Collapse
  | OpinionMsg OView.Action


-- create OPG using a List of OpinionPaths
-- grab the opiner from the paths
init : Int -> List OPath.Model -> (Model, Effects Action)
init key opaths =
  let sorted =
        List.sortBy .score opaths
      opinionId =
        OPath.getOpinionId opaths
      ( opinion, fx ) =
        OView.init opinionId
  in
      ( Model key False sorted opinion
      , Effects.map OpinionMsg fx
      )


initGroups : List OPath.Model -> List (Model, Effects Action)
initGroups allPaths =
  bucketList .opinionId allPaths Dict.empty
    |> Dict.map init
    |> Dict.values


update : Action -> Model -> (Model, Effects Action)
update message model =
  case message of
    Expand ->
      ( { model | expanded = True }
      , OpinionMsg OView.Expand
          |> Task.succeed
          |> Effects.task
      )

    Collapse ->
      ( { model | expanded = False }
      , OpinionMsg OView.Collapse
          |> Task.succeed
          |> Effects.task
      )

    OpinionMsg msg ->
      let (opinion, fx) =
        OView.update msg model.opinion
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

      Nothing -> div [] []

      Just h ->
        let
          opgHeader =
            OPath.viewHeader h (List.length opg.paths)

          others =
            List.map OPath.viewAbbreviated remainder

          opiner =
            OPath.viewOpiner h

          clickAction =
            if opg.expanded then Collapse else Expand

          toggleClass =
            if opg.expanded then "expanded" else "collapsed"

        in
          div [class "opg col m12 l6"]
            [ div [class ("t-card " ++ toggleClass)]
              [ div
                [ class "t-card-title toggles "
                , onClick address clickAction ]
                (opgHeader :: others)
              , div [class "t-card-body"]
                [ opiner
                , OView.view opg.opinion
                ]
              ]
            ]


bucketList : (a -> comparable) -> List a -> Dict.Dict comparable (List a) -> Dict.Dict comparable (List a)
bucketList keyGen opinions dict =
  case opinions of
    o::os ->
      bucketListItem (keyGen o) o dict
      |> bucketList keyGen os
    [] -> dict


bucketListItem : comparable -> a -> Dict.Dict comparable (List a) -> Dict.Dict comparable (List a)
bucketListItem key v dict =
  Dict.insert key (v :: safeGetList key dict) dict


safeGetList : comparable -> Dict.Dict comparable (List a) -> List a
safeGetList key dict = Maybe.withDefault [] (Dict.get key dict)

-- doesn't handle repeat group ids
toDict : List Model -> Dict.Dict Int Model
toDict groups =
  groups
   |> List.map (\group -> (group.groupId, group))
   |> Dict.fromList
