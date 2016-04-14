-- a plotted opinion; an opinion, and where it is (via paths)
module Opinion.Plot
  ( Plot
  , Action
  , init
  , initPlots
  , view
  , update
  , toDict
  , keyFx
  , keyPlot
  , expand
  , collapse
  ) where


import Opinion.Opinion as Opinion exposing (Opinion)
import Opinion.Presenter as Presenter
import Opinion.Path as Path exposing (Path)
import Routes
import Utils.List as ListUtils


import Effects exposing (Effects)
import Html exposing (Html, div, span, text)
import Html.Attributes as Attribute exposing (class)
import Dict


type alias Plot =
    { opinion : Opinion
    , paths : List Path
    , shortestPath : Int
    , expanded : Bool
    }


type Action
  = FetchComplete Opinion


-- Create with a List of OpinionPaths and the Opinion.id
-- We're no longer guaranteed to have a path, as this could be used for
-- un-linked opinions?
init : Int -> List Path -> (Plot, Effects Action)
init opinionId opaths =
  let
    sorted =
      List.sortBy Path.getLength opaths

    shortest =
      List.map Path.getLength sorted
      |> List.minimum
      |> Maybe.withDefault -1

    (opinion, opinionFx) =
      Opinion.fetchById opinionId

  in
    ( { opinion = opinion
      , paths = sorted
      , shortestPath = shortest
      , expanded = False
      }
    , Effects.map FetchComplete opinionFx
    )


update : Action -> Plot -> (Plot, Effects Action)
update message plot =
  case message of

    FetchComplete opinion ->
      ( { plot
        | opinion = Presenter.prepare opinion
        }
      , Effects.none
      )


type alias ViewContext =
  { address : Signal.Address Action
  , routeBuilder : Int -> Routes.Route
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


view : ViewContext -> (Int, Plot) -> Html
view {address, routeBuilder} (k, {opinion, paths, expanded}) =
  let
    (header, expandClass) =
      if expanded then
        (expandedHeader paths, "expanded")
      else
        (collapsedHeader paths, "collapsed")
  in
    div
      [ class <| "opg t-card " ++ expandClass
      , Attribute.key <| toString k ]
      [ header
      , div
        [ class "t-card-body" ]
        [ Presenter.view expanded routeBuilder opinion ]
      ]


collapsedHeader : List Path -> Html
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


expandedHeader : List Path -> Html
expandedHeader paths =
  div
    [ class <| "t-card-title connections" ]
    [ Path.viewPaths paths ]


countBadge : Int -> Html
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


initPlots : List Path -> List (Plot, Effects Action)
initPlots allPaths =
  ListUtils.groupBy .opinionId allPaths
    |> List.map (\(id, paths) -> init id paths)


-- doesn't handle repeat group ids
toDict : (Plot -> comparable) -> List Plot -> Dict.Dict comparable Plot
toDict keyGen plots =
  plots
   |> List.map (\plot -> (keyGen plot, plot))
   |> Dict.fromList


keyFx : (Plot -> comparable) -> (Plot, Effects Action) -> (comparable, Effects Action)
keyFx keyGen (plot, plotFx) =
  (keyGen plot, plotFx)


keyPlot : (Plot -> comparable) -> Plot -> (comparable, Plot)
keyPlot keyGen plot =
  (keyGen plot, plot)
