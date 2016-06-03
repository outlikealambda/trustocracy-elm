-- a plotted opinion; an opinion, and where it is (via paths)
module Opinion.Plot exposing
  ( Plot
  , Msg
  , init
  , initPlots
  , view
  , update
  , toDict
  , keyFx
  , keyPlot
  , expand
  , collapse
  )


import Common.API as API
import Location
import Opinion.Opinion as Opinion exposing (Opinion)
import Opinion.Presenter as Presenter
import Opinion.Path as Path exposing (Path)
import Routes
import Trustee exposing (Trustee)
import Utils.List as ListUtils


import Html exposing (Html, div, span, text)
import Html.Attributes as Attribute exposing (class)
import Json.Encode as Json
import Dict


type alias Plot =
    { opinion : Opinion
    , paths : List Path
    , shortestPath : Int
    , expanded : Bool
    }


type Msg
  = FetchComplete Opinion
  | FetchFailed String
  | SetPath Routes.Route


-- Create with a List of OpinionPaths and the Opinion.id
-- We're no longer guaranteed to have a path, as this could be used for
-- un-linked opinions?
init : Int -> List Path -> (Plot, Cmd Msg)
init opinionId opaths =
  let
    sorted =
      List.sortBy Path.getLength opaths

    shortest =
      List.map Path.getLength sorted
      |> List.minimum
      |> Maybe.withDefault -1

    opinionFx =
      API.fetchOpinionById
        FetchFailed
        FetchComplete
        opinionId

    opinion =
      Opinion.empty

  in
    ( { opinion = { opinion | id = opinionId }
      , paths = sorted
      , shortestPath = shortest
      , expanded = False
      }
    , opinionFx
    )


update : Msg -> Plot -> (Plot, Cmd Msg)
update message plot =
  case message of

    FetchComplete opinion ->
      ( { plot
        | opinion = Presenter.prepare opinion
        }
      , Cmd.none
      )

    FetchFailed err ->
      let
        msg = Debug.log "lost the plot!" err
      in
        ( plot, Cmd.none )

    SetPath route ->
      ( plot, Location.setPath <| Routes.encode route )


type alias ViewContext msg =
  { transform : Msg -> msg
  , assignDelegate : Trustee -> msg
  , readRouteBuilder : Int -> Routes.Route
  }


expand : Plot -> Plot
expand =
  setExpand True


collapse : Plot -> Plot
collapse =
  setExpand False


setExpand : Bool -> Plot -> Plot
setExpand exp plot =
  { plot | expanded = exp }


view : ViewContext msg -> (Int, Plot) -> Html msg
view {transform, assignDelegate, readRouteBuilder} (k, {opinion, paths, expanded}) =
  let
    (header, expandClass) =
      if expanded then
        (expandedHeader paths, "expanded")
      else
        (collapsedHeader paths, "collapsed")
  in
    div
      [ class <| "opg t-card " ++ expandClass
      , Attribute.property "key" (Json.int k)
      ]
      [ header
      , div
        [ class "t-card-body" ]
        [ Presenter.view
            expanded
            assignDelegate
            (transform << SetPath << readRouteBuilder)
            opinion
        ]
      ]


collapsedHeader : List Path -> Html msg
collapsedHeader paths =
  let
    pathHeader =
      case paths of
        h::_ ->
          [ h ]
        [] ->
          []
    badge =
      if List.length paths > 1 then
        countBadge (List.length paths - 1)
      else
        div [] []

  in
    div
      [ class <| "t-card-title connections" ]
      [ badge
      , Path.viewPaths pathHeader
      ]


expandedHeader : List Path -> Html msg
expandedHeader paths =
  div
    [ class <| "t-card-title connections" ]
    [ Path.viewPaths paths ]


countBadge : Int -> Html msg
countBadge c =
  let
    (n, label) =
      case c of
        1 ->
          (1, "other connection")
        n ->
          (n, "other connections")
  in
    div
      [ class "path-count numbered-badge" ]
      [ span
        [ class "numbered-count" ]
        [ text <| toString n ]
      , span
        [ class "numbered-label" ]
        [ text label ]
      ]


initPlots : List Path -> List (Plot, Cmd Msg)
initPlots allPaths =
  ListUtils.groupBy .opinionId allPaths
    |> List.map (\(id, paths) -> init id paths)


-- doesn't handle repeat group ids
toDict : (Plot -> comparable) -> List Plot -> Dict.Dict comparable Plot
toDict keyGen plots =
  plots
   |> List.map (\plot -> (keyGen plot, plot))
   |> Dict.fromList


keyFx : (Plot -> comparable) -> (Plot, Cmd Msg) -> (comparable, Cmd Msg)
keyFx keyGen (plot, plotFx) =
  (keyGen plot, plotFx)


keyPlot : (Plot -> comparable) -> Plot -> (comparable, Plot)
keyPlot keyGen plot =
  (keyGen plot, plot)
