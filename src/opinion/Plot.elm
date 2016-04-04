-- a plotted opinion; an opinion, and where it is (via paths)
module Opinion.Plot
  ( Plot
  , Action
  , initPlots
  , view
  , update
  , toDict
  ) where


import Opinion.Opinion as Opinion exposing (Opinion)
import Opinion.Presenter as Presenter
import Opinion.Path as Path exposing (Path)
import Routes
import Utils.List as ListUtils


import Effects exposing (Effects)
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class)
import Dict


type alias Plot =
    { opinion : Opinion
    , paths : List Path, shortestPath : Int
    , expanded : Bool
    }


type Action
  = Expand
  | Collapse
  | FetchComplete Opinion


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
      |> Maybe.withDefault 0

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
        | opinion = Presenter.prepare <| Debug.log "opinion: " opinion
        }
      , Effects.none
      )

    Expand ->
      ( { plot
        | expanded = True
        -- , opinion = Presenter.expand group.opinion
        }
      , Effects.none )

    Collapse ->
      ( { plot
        | expanded = False
        -- , opinion = Presenter.collapse group.opinion
        }
      , Effects.none )


type alias ViewContext =
  { address : Signal.Address Action
  , routeBuilder : Int -> Routes.Route
  }


view : ViewContext -> Plot -> Html
view {address, routeBuilder} {opinion, paths, expanded} =
  div
    [ class "opg t-card"]
    [ viewHeader paths expanded
    , div
      [ class "t-card-body" ]
      [ Presenter.viewCollapsed routeBuilder opinion ]
    ]


viewHeader : List Path -> Bool -> Html
viewHeader paths expanded =
  let
    pathHeader =
      case paths of
        h::_ ->
          [ h ]
        [] ->
          []
    badge =
      case List.length paths - 1 of
        0 -> div [] []
        n -> countBadge n

  in
    div
      [ class <| "t-card-title connections" ]
      [ badge
      , Path.viewPaths pathHeader
      ]


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
