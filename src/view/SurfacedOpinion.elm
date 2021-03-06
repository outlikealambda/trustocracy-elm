module View.SurfacedOpinion exposing
  ( view
  , Context
  )


import Common.Remote exposing
  ( Remote
    ( NoRequest
    , Requested
    , Retrieved
    )
  )


import Model.SurfacedOpinion.SurfacedOpinion exposing (SurfacedOpinion(..))
import Model.SurfacedOpinion.Link exposing (Link)
import Model.Opinion.Metrics exposing (Metrics)
import Model.Opinion.Opinion exposing(Opinion)
import Model.Question.Question as Question exposing (Question)


import Utils.Date as DateUtils


import View.Author as AuthorView
import View.Opinion as OpinionView
import View.Path as PathView
import View.Question.Metrics as MetricsView


import Html exposing (Html)
import Html.Attributes as Attrs exposing (class)
import Html.Events as Events
import Date exposing (Date)


type alias OpinionId = Int
type alias Qid = Int
type alias Key = Int


type alias Context msg =
  { showAll : () -> msg
  , readMore : () -> msg
  -- , next : () -> msg
  , questions : List Question
  , isExpanded : Bool
  }


view : Context msg -> SurfacedOpinion -> Html msg
view context surfacedOpinion =
  case surfacedOpinion of
    Unconnected opinion ->
      public context opinion

    Connected opinion link ->
      connected context opinion link


public : Context msg -> Opinion -> Html msg
public context opinion =
  Html.div
    [ class "topic-opinion disjoint cf" ]
    <|
      [ Html.div
        [ class "topic-opinion-header cf" ]
        [ basicHeader context opinion ]
      ]
      ++
        body context opinion


basicHeader : Context msg -> Opinion -> Html msg
basicHeader context {metrics, author, influence} =
  Html.div
    [ class "basic-header" ]
    [ viewMetrics context metrics
    , AuthorView.connected author
    , viewInfluence influence
    ]


connected : Context msg -> Opinion -> Link -> Html msg
connected context opinion link =
  let
    buildPathElements paths =
      List.map PathView.view paths

  in
    Html.div
      [ class "topic-opinion connected cf" ]
      <|
        [ Html.div
          [ class "topic-opinion-header cf" ]
          [ Html.div
            [ class "paths" ]
            (buildPathElements link.userLink)
          , basicHeader context opinion
          ]
        ]
        ++
          body context opinion


body : Context msg -> Opinion -> List (Html msg)
body context opinion =
  case context.isExpanded of
    True ->
      [ OpinionView.text True opinion
      , lastUpdated opinion.created
      , Html.map context.showAll showAll
      ]

    False ->
      [ OpinionView.text False opinion ]
      ++ [ Html.map context.readMore readMoreButton ]



readMoreButton : Html ()
readMoreButton =
  Html.a
    [ class "read-more"
    , Events.onClick ()
    ]
    [ Html.text "read more..."
    ]


showAll : Html ()
showAll =
  Html.a
    [ class "show-all"
    , Events.onClick ()
    ]
    [ Html.text "show all..."
    ]


lastUpdated : Date -> Html msg
lastUpdated date =
  Html.div
    [ class "last-updated" ]
    [ Html.text <| DateUtils.asString date ]


viewInfluence : Remote Int -> Html msg
viewInfluence remoteInfl =
  case remoteInfl of
    NoRequest ->
      Html.div
        [ class "influence no-request" ]
        [ Html.text "Couldn't calculate influence :("]

    Requested ->
      Html.div
        [ class "influence requested" ]
        [ Html.text "Calculating..." ]

    Retrieved influence ->
      Html.div
        [ class "influence retrieved" ]
        [ Html.text <| toString influence ++ " connected users" ]


viewMetrics : Context msg -> Remote Metrics -> Html msg
viewMetrics context remoteMetrics =
  case remoteMetrics of
    Retrieved m ->
      MetricsView.view context.questions m
    _ ->
      Html.div [] []
