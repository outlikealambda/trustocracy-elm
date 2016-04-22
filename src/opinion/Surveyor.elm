-- displays plotted opinions
module Opinion.Surveyor
  ( Surveyor
  , Action
    ( Init
    )
  , empty
  , view
  , navButton
  , update
  , focus
  , blur
  ) where


import Effects exposing (Effects)
import Html exposing (Html, div, text, h4)
import Html.Attributes exposing (class)
import Dict


import ActiveUser
import Common.API as API
import Opinion.Plot as Plot exposing (Plot)
import Opinion.Path as Path
import Routes
import Topic.Model as Topic exposing (Topic)


type alias Key = Int

type alias Paths = List Path.Path

type alias Surveyor =
  { rawPaths : Paths
  , buckets : Dict.Dict Key Plot-- opinion paths bucketed by key
  , longestPlotPath : Int
  , isSurveyed : Bool
  , zoom : Zoom
  , topic : Topic
  }


type Action
  = SetConnected Paths
  | SetUnconnected (List Int)
  | Init Topic ActiveUser.ActiveUser
  | PlotMsg Key Plot.Action


type Zoom
  = Focus Int
  | Blur


empty : Surveyor
empty =
  { rawPaths = []
  , buckets = Dict.empty
  , longestPlotPath = 0
  , isSurveyed = False
  , zoom = Blur
  , topic = Topic.empty
  }


update : Action -> Surveyor -> (Surveyor, Effects Action)
update message model =
  case message of

    Init topic activeUser ->
      let
        fx =
          case activeUser of
            ActiveUser.LoggedOut ->
              API.fetchIdsByTopic
                (SetUnconnected << Maybe.withDefault [])
                topic

            ActiveUser.LoggedIn _ ->
              API.fetchConnected
                (SetConnected << Maybe.withDefault [])
                topic
      in
        ( { model
          | topic = topic
          , buckets = Dict.empty
          }
        , fx
        )

    SetConnected opaths ->
      let
        keyGen =
          .id << .opinion

        plotPairs =
          Plot.initPlots opaths

        keyedPlotsFxs =
          List.map (Plot.keyFx keyGen) plotPairs
          |> List.map (\(k, fx) -> Effects.map (PlotMsg k) fx)

        connectedBuckets =
          List.map fst plotPairs
          |> List.map (Plot.keyPlot keyGen)
          |> Dict.fromList

        longestPlotPath =
          Dict.values connectedBuckets
          |> List.map .shortestPath
          |> List.maximum
          |> Maybe.withDefault 0

      in
        ( { model
          | rawPaths = opaths
          , buckets = Dict.union connectedBuckets model.buckets
          , longestPlotPath = longestPlotPath
          }
        , API.fetchIdsByTopic (SetUnconnected << Maybe.withDefault []) model.topic
          :: keyedPlotsFxs
          |> Effects.batch
        )

    SetUnconnected ids ->
      let
        keyGen =
          .id << .opinion

        isOpinionFetched =
          flip Dict.member model.buckets

        -- List (Plot, Effects Plot.Action)
        plotPairs =
          -- avoid re-fetching opinions we already have via SetConnected
          List.filter (not << isOpinionFetched) (Debug.log "set unconnected" ids)
          |> List.map (\id -> Plot.init id [])

        -- end up with a List (Effects (PlotMsg key Plot.Action))
        keyedPlotsFxs =
          List.map (Plot.keyFx keyGen) plotPairs
          |> List.map (\(k, fx) -> Effects.map (PlotMsg k) fx)

        -- end up with a Dict (key, Plot)
        unconnectedBuckets =
          List.map fst plotPairs
          |> List.map (Plot.keyPlot keyGen)
          |> Dict.fromList

      in
        ( { model
          | buckets = Dict.union unconnectedBuckets model.buckets
          , isSurveyed = True
          }
        , Effects.batch keyedPlotsFxs
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


focus : Int -> Surveyor -> Surveyor
focus target surveyor =
  { surveyor | zoom = Focus target }


blur : Surveyor -> Surveyor
blur surveyor =
  { surveyor | zoom = Blur }


type alias ViewContext =
  { address : Signal.Address Action
  , readRouteBuilder : Int -> Routes.Route
  , showAllRoute : Routes.Route
  }


view : ViewContext -> Surveyor -> Html
view context {zoom, buckets, longestPlotPath} =
  let
    focusTarget target (t, p) =
      if t == target then
        (t, Plot.expand p)
      else
        (t, p)
    (zoomedClass, zoomedPlots) =
      case zoom of
        Blur ->
          ("blurred", Dict.toList buckets)
        Focus target ->
          ("focused", List.map (focusTarget target) (Dict.toList buckets))
  in
    div
      [ class <| "surveyor " ++ zoomedClass ]
      <| showAll context.showAllRoute
      :: (viewAllGrouped context longestPlotPath zoomedPlots)


showAll : Routes.Route -> Html
showAll showAllRoute =
  div
    [ class "show-all-wrapper cf" ]
    [ Html.span
      [ class "show-all"
      , Routes.goToRoute showAllRoute
      ]
      [ Html.text "Show all opinions" ]
    ]


viewAllGrouped : ViewContext -> Int -> List (Int, Plot) -> List Html
viewAllGrouped context longestPlotPath plots=
  let
    sectionConstructors =
      List.map (viewPlotSection context) [0..longestPlotPath]
    connectedSections =
      -- mapping a value (here, a list) over a list of functions is a little
      -- bit unwieldy
      List.map ((|>) plots) sectionConstructors
    unconnectedSection =
      [ viewPlotSection context -1 plots ]
    sections =
      connectedSections ++ unconnectedSection
      |> List.filterMap identity
  in
    sections


viewPlotSection : ViewContext -> Int -> List (Key, Plot) -> Maybe Html
viewPlotSection address pathLength keyPlots =
  let
    groupDivs =
      groupsOfLength pathLength keyPlots
        |> List.map (viewPlot address)
    header =
      h4
        [ class "group-section-header" ]
        [ text <| degreeLabel pathLength ]
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


viewPlot : ViewContext -> (Key, Plot) -> Html
viewPlot {address, readRouteBuilder} (key, opg) =
  Plot.view
    { address = Signal.forwardTo address (PlotMsg key)
    , readRouteBuilder = readRouteBuilder
    }
    (key, opg)


groupsOfLength : Int -> List (Key, Plot) -> List (Key, Plot)
groupsOfLength pathLength groups =
  List.filter
    (\keyPlot -> pathLength == (.shortestPath <| snd keyPlot))
    groups


degreeLabel : Int -> String
degreeLabel n =
  (degreeLabelHead n) ++ (degreeLabelTail n)


degreeLabelHead : Int -> String
degreeLabelHead n =
  case n of
    -1 -> "Unconnected"
    0 -> "Direct"
    1 -> "1st degree"
    2 -> "2nd degree"
    3 -> "3rd degree"
    k -> (toString k) ++ "th degree"


degreeLabelTail : Int -> String
degreeLabelTail n =
  case n of
    -1 -> ""
    n -> " connections"


navButton : Surveyor -> Html
navButton {buckets, isSurveyed} =
  let
    count = Dict.size buckets
  in
    if isSurveyed then
      div
        [ class "connect fetched" ]
        [ text <| (toString count) ++ " Opinions" ]
    else
      div [] []
