-- displays plotted opinions
module Opinion.Surveyor
  ( Surveyor
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

import Opinion.Plot as Plot exposing (Plot)
import Opinion.Path as Path
import User exposing (User)
import Topic.Model exposing (Topic)


type alias Key = Int
type alias Paths = List Path.Model


type alias Surveyor =
  { rawPaths : Paths
  , buckets : Dict.Dict Key Plot-- opinion paths bucketed by key
  , longestPlotPath : Int
  , pathsFetched : Bool
  }


type Action
  = SetRaw Paths
  | PlotMsg Key Plot.Action


empty : Surveyor
empty =
  { rawPaths = []
  , buckets = Dict.empty
  , longestPlotPath = 0
  , pathsFetched = False
  }


init : User -> Topic -> (Surveyor, Effects Action)
init user topic =
  ( empty
  , fetchPlotted topic user
  )


update : Action -> Surveyor -> (Surveyor, Effects Action)
update message model =
  case message of
    SetRaw opaths ->
      case opaths of
        opaths ->
          let
            keyGen = .id << .opinion
            -- super ugly, fix
            -- this gets tricky, because we need to route the PlotMsg to the
            -- appropriate Dict.value, so we need to map to the appropriate key
            (plots, plotFxs) =
              Plot.initPlots opaths
                |> List.map
                  (\(plot, plotFx) ->
                    ( plot
                    , Effects.map (PlotMsg (keyGen plot)) plotFx
                    )
                  )
                |> List.unzip
            buckets =
              Plot.toDict keyGen plots
          in
            ( { model
              | rawPaths = opaths
              , pathsFetched = True
              , buckets = buckets
              , longestPlotPath =
                Dict.values buckets
                |> List.map .shortestPath
                |> List.maximum
                |> Maybe.withDefault 0
              }
            , Effects.batch plotFxs
            )

    PlotMsg key subMsg ->
      case Dict.get key model.buckets of

        Nothing ->
          ( model
          , Effects.none )

        Just bucket ->
          let
            (updatedBucket, fx) =
              Plot.update subMsg bucket
            updatedBuckets =
              Dict.insert key updatedBucket model.buckets
          in
            ( { model | buckets = updatedBuckets }
            , Effects.map (PlotMsg key) fx
            )


fetchPlotted : Topic -> User -> Effects Action
fetchPlotted topic user =
  buildPlottedUrl topic.id user.id
    |> Http.get opathsDecoder
    |> Task.toMaybe
    |> Task.map (Maybe.withDefault [])
    |> Task.map SetRaw
    |> Effects.task


opathsDecoder : Json.Decoder Paths
opathsDecoder =
  "paths" := Json.list Path.decoder


buildPlottedUrl : Int -> Int -> String
buildPlottedUrl tid uid =
  String.concat
    [ "http://localhost:3714/api/user/"
    , toString uid
    , "/topic/"
    , toString tid
    , "/opinions"
    ]


view : Signal.Address Action -> Surveyor -> List Html
view address {buckets, longestPlotPath} =
  let
    sectionCreators =
      List.map (viewPlotSection address) [0..longestPlotPath]
    maybeSections =
      -- mapping a value (here, a list) over a list of functions is a little
      -- bit tricky
      List.map ((|>) (Dict.toList buckets)) sectionCreators
    sections =
      List.filterMap identity maybeSections
  in
    sections


viewPlotSection : Signal.Address Action -> Int -> List (Key, Plot) -> Maybe Html
viewPlotSection address pathLength keyPlots =
  let
    groupDivs =
      groupsOfLength pathLength keyPlots
        |> List.map (viewPlot address)
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


viewPlot : Signal.Address Action -> (Key, Plot) -> Html
viewPlot address (key, opg) =
  Plot.view (Signal.forwardTo address (PlotMsg key)) opg


groupsOfLength : Int -> List (Key, Plot) -> List (Key, Plot)
groupsOfLength pathLength groups =
  List.filter
    (\keyPlot -> pathLength == (.shortestPath <| snd keyPlot))
    groups


degreeLabel : Int -> String
degreeLabel n =
  case n of
    0 -> "Direct"
    1 -> "1st degree"
    2 -> "2nd degree"
    3 -> "3rd degree"
    k -> (toString k) ++ "th degree"


navButton : Surveyor -> Html
navButton {buckets, pathsFetched} =
  let
    count = Dict.size buckets
  in
    if pathsFetched then
      div
        [ class "connect fetched" ]
        [ text <| (toString count) ++ " Connected Opinions" ]
    else
      div [] []
