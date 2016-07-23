module View.Connection exposing
  ( connection
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


type alias Context msg =
  { showAll : () -> msg
  , readMore : OpinionId -> msg
  , next : Update.Msg -> msg
  , questions : List Question
  }


connection : Context msg -> Connection -> Html msg
connection context {opinion, paths, status, assessor} =
  case status of

    Expandable.Expanded ->
      Html.div
        [ class "connection cf" ]
        [ Html.div
          [ class "connection-header cf" ]
          [ Html.div
            [ class "paths" ]
            ( List.map PathView.view paths )
          , AuthorView.connection opinion.author
          ]
        , OpinionView.text True opinion
        , lastUpdated opinion.created
        , AssessorView.questions context.questions assessor
          |> Html.App.map Update.DelegateToAssessor
          |> Html.App.map context.next
        , Html.App.map context.showAll showAll
        ]

    Expandable.Collapsed ->
      Html.div
        [ class "connection cf" ]
        [ Html.div
          [ class "connection-header cf" ]
          [ Html.div
            [ class "paths" ]
            ( List.head paths
              |> Maybe.map PathView.view
              |> Maybe.map ListUtils.singleton
              |> Maybe.withDefault []
            )
          , AuthorView.connection opinion.author
          ]
        , OpinionView.text False opinion
        , Html.App.map context.readMore <| readMore opinion.id
        ]


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
