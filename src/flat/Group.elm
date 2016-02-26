module Flat.Group
  ( Group
  , Action
  , initGroups
  , view
  , update
  , toDict
  ) where


import Flat.Opinion as Opinion exposing (Opinion)
import Flat.Presenter as Presenter
import Flat.Path as Path


import Effects exposing (Effects)
import Html exposing (Html, Attribute, div, span, text, button)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Dict


type alias Group =
    { groupId : Int
    , paths : List Path.Model
    , shortestPath : Int
    , expanded : Bool
    , opinion : Opinion
    }


type Action
  = Expand
  | Collapse
  | FetchComplete Opinion


-- create OPG using a List of OpinionPaths
-- grab the opiner from the paths
init : Int -> List Path.Model -> (Group, Effects Action)
init key opaths =
  let
    sorted =
      List.sortBy Path.getLength opaths

    opinionId =
      Path.getOpinionId opaths

    shortest =
      List.map Path.getLength sorted
      |> List.minimum
      |> Maybe.withDefault 0

  in
    ( { groupId = key
      , paths = sorted
      , shortestPath = shortest
      , expanded = False
      , opinion = Opinion.empty
      }
    , Opinion.fetchById opinionId
      |> Effects.map FetchComplete
    )


update : Action -> Group -> (Group, Effects Action)
update message group =
  case message of

    FetchComplete opinion ->
      ( { group
        | opinion = Presenter.prepare opinion
        }
      , Effects.none
      )

    Expand ->
      ( { group
        | expanded = True
        , opinion = Presenter.expand group.opinion
        }
      , Effects.none )

    Collapse ->
      ( { group
        | expanded = False
        , opinion = Presenter.collapse group.opinion
        }
      , Effects.none )


view : Signal.Address Action -> Group -> Html
view = viewByOpinion


viewByOpinion : Signal.Address Action -> Group -> Html
viewByOpinion address {opinion, paths, expanded} =
  let

    remainder =
      if expanded then
        List.tail paths |> Maybe.withDefault []
      else []

  in
    case List.head paths of

      -- somehow, no paths, so render nothing
      Nothing -> div [] []

      -- hooray, at least one path
      Just h ->
        let
          groupHeader =
            Path.viewHeader h (List.length paths)

          others =
            List.map Path.viewAbbreviated remainder

          -- this could go into Presenter if we add an Opinion.User
          opiner =
            div
              [ class "opinion-header cf" ]
              [ div
                [ class "opiner" ]
                [ text <| Path.getOpinerName h ]
              , div
                [ class "numbered-badge influence" ]
                [ span
                  [ class "numbered-count" ]
                  [ text <| toString opinion.influence ]
                , span
                  [ class "numbered-label" ]
                  [ text "influenced people" ]
                ]
              ]

          clickAction =
            if expanded then Collapse else Expand

          toggleClass =
            if expanded then
              "expanded"
            else "collapsed"

        in
          div
            [ class ("opg t-card " ++ toggleClass) ]
            [ div
              [ class "t-card-title toggles"
              , onClick address clickAction ]
              ( groupHeader :: others )
            , div [class "t-card-body"]
              [ opiner
              , Presenter.view opinion
              ]
            ]


initGroups : List Path.Model -> List (Group, Effects Action)
initGroups allPaths =
  bucketList .opinionId allPaths Dict.empty
    |> Dict.map init
    |> Dict.values



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
