module View.Connection exposing
  ( linked
  , authorQualifications
  , Context
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
  , readMore : OpinionId -> msg
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
            , Html.App.map context.readMore <| readMore opinion.id
            ]
  in
    Html.div
      [ class "disjoint cf" ]
      childElements


linked : Context msg -> Connection -> Maybe (Html msg)
linked context {opinion, inflation, assessor, userLink} =
  let
    childElements =
      case inflation of
        Expandable.Expanded ->
          [ OpinionView.text True opinion
          , lastUpdated opinion.created
          , AssessorView.questions context.questions assessor
            |> Html.App.map Update.DelegateToAssessor
            |> Html.App.map context.next
          , Html.App.map context.showAll showAll
          ]

        Expandable.Collapsed ->
          [ OpinionView.text False opinion
          , Html.App.map context.readMore <| readMore opinion.id
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


readMore : OpinionId -> Html OpinionId
readMore oid =
  Html.a
    [ class "read-more"
    , Events.onClick oid
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
