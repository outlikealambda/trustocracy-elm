module Opinion.Connector
  ( Connector
  , Action
  , empty
  , init
  , view
  , navButton
  , update
  ) where

import Effects exposing (Effects)
import Task
import Html exposing (Html, div, text, h4)
import Html.Attributes exposing (class)
import Json.Decode as Json exposing ((:=))
import Dict
import String
import Http

import Opinion.Group as Group exposing (Group)
import Opinion.Path as Path
import User exposing (User)
import Topic.Model exposing (Topic)


type alias Key = Int
type alias Paths = List Path.Model


type alias Connector =
  { rawPaths : Paths
  , buckets : Dict.Dict Key Group-- opinion paths bucketed by key
  , longestGroupPath : Int
  , pathsFetched : Bool
  }


type Action
  = SetRaw Paths
  | GroupMsg Key Group.Action


empty : Connector
empty =
  { rawPaths = []
  , buckets = Dict.empty
  , longestGroupPath = 0
  , pathsFetched = False
  }


init : User -> Topic -> (Connector, Effects Action)
init user topic =
  ( empty
  , getConnectedOpinions topic user
  )


update : Action -> Connector -> (Connector, Effects Action)
update message model =
  case message of
    SetRaw opaths ->
      case opaths of
        opaths ->
          let
            (groups, fxs) =
              Group.initGroups opaths
                |> List.map (\(g, fx) -> (g, Effects.map (GroupMsg g.groupId) fx))
                |> List.unzip
            buckets =
              Group.toDict groups
          in
            ( { model
              | rawPaths = opaths
              , pathsFetched = True
              , buckets = buckets
              , longestGroupPath =
                Dict.values buckets
                |> List.map .shortestPath
                |> List.maximum
                |> Maybe.withDefault 0
              }
            , Effects.batch fxs
            )

    GroupMsg key subMsg ->
      case Dict.get key model.buckets of

        Nothing ->
          ( model
          , Effects.none )

        Just bucket ->
          let
            (updatedBucket, fx) =
              Group.update subMsg bucket
            updatedBuckets =
              Dict.insert key updatedBucket model.buckets
          in
            ( { model | buckets = updatedBuckets }
            , Effects.map (GroupMsg key) fx
            )


getConnectedOpinions : Topic -> User -> Effects Action
getConnectedOpinions topic user =
  buildConnectedOpinionsUrl topic.id user.id
    |> Http.get opathsDecoder
    |> Task.toMaybe
    |> Task.map (Maybe.withDefault [])
    |> Task.map SetRaw
    |> Effects.task


opathsDecoder : Json.Decoder Paths
opathsDecoder =
  "paths" := Json.list Path.decoder


buildConnectedOpinionsUrl : Int -> Int -> String
buildConnectedOpinionsUrl tid uid =
  String.concat
    [ "http://localhost:3714/api/user/"
    , toString uid
    , "/topic/"
    , toString tid
    , "/opinions"
    ]


view : Signal.Address Action -> Connector -> List Html
view address {buckets, longestGroupPath} =
  let
    sectionCreators =
      List.map (viewGroupSection address) [0..longestGroupPath]
    maybeSections =
      -- mapping a value (here, a list) over a list of functions is a little
      -- bit tricky
      List.map ((|>) (Dict.toList buckets)) sectionCreators
    sections =
      List.filterMap identity maybeSections
  in
    sections


viewGroupSection : Signal.Address Action -> Int -> List (Key, Group) -> Maybe Html
viewGroupSection address pathLength keyGroups =
  let
    groupDivs =
      groupsOfLength pathLength keyGroups
        |> List.map (viewGroup address)
    header =
      h4
        [ class "group-section-header" ]
        [ text <| (degreeLabel pathLength) ++ " connections"]
    section =
      div
        [ class "group-section" ]
        (header :: groupDivs)
  in
    case groupDivs of
      [] ->
        Nothing
      divs ->
        Just section


viewGroup : Signal.Address Action -> (Key, Group) -> Html
viewGroup address (key, opg) =
  Group.view (Signal.forwardTo address (GroupMsg key)) opg


groupsOfLength : Int -> List (Key, Group) -> List (Key, Group)
groupsOfLength pathLength groups =
  List.filter
    (\keyGroup -> pathLength == (.shortestPath <| snd keyGroup))
    groups


degreeLabel : Int -> String
degreeLabel n =
  case n of
    0 -> "Direct"
    1 -> "1st degree"
    2 -> "2nd degree"
    3 -> "3rd degree"
    k -> (toString k) ++ "th degree"


navButton : Connector -> Html
navButton {buckets, pathsFetched} =
  let
    count = Dict.size buckets
  in
    if pathsFetched then
      div
        [ class "connect fetched" ]
        [ text <| (toString count) ++ " connected opinions" ]
    else
      div
        []
        []
