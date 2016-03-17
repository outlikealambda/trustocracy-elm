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
import Opinion.Path as Path
import Utils.List as ListUtils


import Effects exposing (Effects)
import Html exposing (Html, Attribute, div, span, text, button)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Dict


type alias Plot =
    { opinion : Opinion
    , paths : List Path.Model
    , shortestPath : Int
    , expanded : Bool
    }


type Action
  = Expand
  | Collapse
  | FetchComplete Opinion


-- Create with a List of OpinionPaths and the Opinion.id
-- We're no longer guaranteed to have a path, as this could be used for
-- un-linked opinions?
init : Int -> List Path.Model -> (Plot, Effects Action)
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
update message plot = -- because group turns cyan in atom
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


view : Signal.Address Action -> Plot -> Html
view = viewByOpinion


viewByOpinion : Signal.Address Action -> Plot -> Html
viewByOpinion address {opinion, paths, expanded} =
  let

    remainder =
      if expanded then
        List.tail paths |> Maybe.withDefault []
      else []

  in
    case List.head paths of

      -- no paths, maybe because of Reader?
      Nothing ->
        div
          [ class "connector" ] [ text "no connex" ]

      -- hooray, at least one path
      Just h ->
        let
          groupHeader =
            Path.viewHeader h (List.length paths)

          others =
            List.map Path.viewAbbreviated remainder

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
              [ class "t-card-title connections toggles"
              , onClick address clickAction ]
              ( groupHeader :: others )
            , div [class "t-card-body"]
              [ Presenter.view opinion
              ]
            ]


initPlots : List Path.Model -> List (Plot, Effects Action)
initPlots allPaths =
  ListUtils.groupBy .opinionId allPaths
    |> List.map (\(id, paths) -> init id paths)


-- doesn't handle repeat group ids
toDict : (Plot -> comparable) -> List Plot -> Dict.Dict comparable Plot
toDict keyGen plots =
  plots
   |> List.map (\plot -> (keyGen plot, plot))
   |> Dict.fromList
