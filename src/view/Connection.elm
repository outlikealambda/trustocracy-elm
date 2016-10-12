module View.Connection exposing
  ( linked
  , authorQualifications
  , Context
  )


import Common.Remote exposing
  ( Remote
    ( NoRequest
    , Requested
    , Retrieved
    )
  )

import Model.Connection as Connection exposing (Connection)
import Model.Extend.Expandable as Expandable exposing (Expandable)
import Model.Question.Question exposing (Question)

import Update.Connection as Update

import Utils.List as ListUtils
import Utils.Date as DateUtils


import View.Question.Assessor as AssessorView
import View.Author as AuthorView
import View.Opinion as OpinionView
import View.Path as PathView


import Html exposing (Html)
import Html.App
import Html.Attributes as Attrs exposing (class)
import Html.Events as Events
import Date exposing (Date)


type alias OpinionId = Int
type alias Qid = Int
type alias Key = Int


type alias Context msg =
  { showAll : () -> msg
  , readMore : Update.Msg -> msg
  , next : Update.Msg -> msg
  , questions : List Question
  }


authorQualifications : Context msg -> Connection -> Html msg
authorQualifications context {opinion, inflation, assessor} =
  let
    childElements =
      case inflation of
        Expandable.Expanded ->
            [ OpinionView.kitchenSink True opinion
            , lastUpdated opinion.created
            , AssessorView.questions context.questions assessor
              |> Html.App.map Update.DelegateToAssessor
              |> Html.App.map context.next
            , Html.App.map context.showAll showAll
            ]

        Expandable.Collapsed ->
            [ OpinionView.kitchenSink False opinion
            , Html.App.map context.readMore readMoreButton
            ]
  in
    Html.div
      [ class "disjoint cf" ]
      childElements

-- Just (Html msg) if the Opinion is connected
-- Nothing if the opinion is not connected
linked : Context msg -> Connection -> Maybe (Html msg)
linked context {opinion, inflation, assessor, userLink, influence} =
  let
    childElements =
      case inflation of
        Expandable.Expanded ->
          [ viewInfluence influence
          , OpinionView.text True opinion
          , lastUpdated opinion.created
          , AssessorView.questions context.questions assessor
            |> Html.App.map Update.DelegateToAssessor
            |> Html.App.map context.next
          , Html.App.map context.showAll showAll
          ]

        Expandable.Collapsed ->
          [ viewInfluence influence
          , OpinionView.text False opinion
          , Html.App.map context.readMore readMoreButton
          ]

    buildPathElements paths =
      case inflation of
        Expandable.Expanded ->
          List.map PathView.view paths

        Expandable.Collapsed ->
          List.head paths
            |> Maybe.map PathView.view
            |> Maybe.map ListUtils.singleton
            |> Maybe.withDefault []

    build pathElements =
      Html.div
        [ class "connection cf" ]
        ( ( Html.div
            [ class "connection-header cf" ]
            [ Html.div
              [ class "paths" ]
              pathElements
            , AuthorView.connection opinion.author
            ]
          )
          :: childElements
        )

  in
    Maybe.map (build << buildPathElements) userLink


readMoreButton : Html Update.Msg
readMoreButton =
  Html.a
    [ class "read-more"
    , Events.onClick ()
    ]
    [ Html.text "read more..."
    ]
    |> Html.App.map (always Update.Zoom)


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
        [ Html.text "Calculating Influence..." ]
    Retrieved influence ->
      Html.div
        [ class "influence retrieved" ]
        [ Html.text <| toString influence ++ " connected users" ]
