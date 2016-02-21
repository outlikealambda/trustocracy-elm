module Opinion.Group
  ( Group
  , Action
  , initGroups
  , view
  , update
  , toDict
  ) where


import Opinion.View as View
import Opinion.Path as Path


import Effects exposing (Effects)
import Task
import Html exposing (Html, Attribute, div, span, text, button)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Dict


type alias Group =
  { groupId : Int
  , expanded : Bool
  , paths : List Path.Model
  , opinion : View.Model
  , shortestPath : Int
  }


type Action
  = Expand
  | Collapse
  | OpinionMsg View.Action


-- create OPG using a List of OpinionPaths
-- grab the opiner from the paths
init : Int -> List Path.Model -> (Group, Effects Action)
init key opaths =
  let
    sorted =
      List.sortBy Path.getLength opaths
    opinionId =
      Path.getOpinionId opaths
    ( opinion, fx ) =
      View.init opinionId
  in
    ( { groupId = key
      , expanded = False
      , paths = sorted
      , opinion = opinion
      , shortestPath =
        List.map Path.getLength sorted
        |> List.minimum
        |> Maybe.withDefault 0
      }
    , Effects.map OpinionMsg fx
    )


initGroups : List Path.Model -> List (Group, Effects Action)
initGroups allPaths =
  bucketList .opinionId allPaths Dict.empty
    |> Dict.map init
    |> Dict.values


update : Action -> Group -> (Group, Effects Action)
update message model =
  case message of
    Expand ->
      ( { model | expanded = True }
      , OpinionMsg View.Expand
          |> Task.succeed
          |> Effects.task
      )

    Collapse ->
      ( { model | expanded = False }
      , OpinionMsg View.Collapse
          |> Task.succeed
          |> Effects.task
      )

    OpinionMsg msg ->
      let (opinion, fx) =
        View.update msg model.opinion
      in
        ( { model | opinion = opinion }
        , Effects.map OpinionMsg fx
        )


view : Signal.Address Action -> Group -> Html
view = viewByOpinion


viewByOpinion : Signal.Address Action -> Group -> Html
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
            Path.viewHeader h (List.length opg.paths)

          others =
            List.map Path.viewAbbreviated remainder

          opiner =
            Path.viewOpiner h

          clickAction =
            if opg.expanded then Collapse else Expand

          toggleClass =
            if opg.expanded then "expanded" else "collapsed"

        in
          div
            [ class ("opg t-card " ++ toggleClass) ]
            [ div
              [ class "t-card-title toggles "
              , onClick address clickAction ]
              (opgHeader :: others)
            , div [class "t-card-body"]
              [ opiner
              , View.view opg.opinion
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
toDict : List Group -> Dict.Dict Int Group
toDict groups =
  groups
   |> List.map (\group -> (group.groupId, group))
   |> Dict.fromList
