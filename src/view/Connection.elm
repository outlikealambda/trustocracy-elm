module View.Connection exposing
  ( view
  , Context
  )


import Model.Connection as Connection exposing (Connection)
import Model.Expandable as Expandable exposing (Expandable)
import Model.Question.Answer exposing (Answer)
import Model.Question.Question exposing (Question)

import Update.Connection as Update

import Utils.List as ListUtils
import Utils.Date as DateUtils


import View.Author as AuthorView
import View.Opinion as OpinionView
import View.Path as PathView
import View.Question.Question as QuestionView


import Html exposing (Html)
import Html.App
import Html.Attributes as Attrs exposing (class)
import Html.Events as Events
import Date exposing (Date)
import Dict exposing (Dict)


type alias OpinionId = Int
type alias Qid = Int

type alias Context msg =
  { showAll : () -> msg
  , readMore : OpinionId -> msg
  , next : Int -> Update.Msg -> msg
  , questions : List Question
  }


view : Context msg -> Connection -> Html msg
view context {opinion, paths, status, answers} =
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
        , Html.div
          [ class "questions" ]
          ( viewQuestions answers context.questions <| context.next opinion.id )
        , Html.App.map context.showAll showAll
        ]

    Expandable.Collapsed ->
      Html.div
        [ class "connection cf" ]
        [ Html.div
          [ class "connection-header cf" ]
          [ Html.div
            [ class "paths" ]
            ( List.head (Debug.log "all paths" paths)
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


viewQuestions : Dict Qid Answer -> List Question -> (Update.Msg -> msg) -> List (Html msg)
viewQuestions answers questions toExplorerMsg =
  let
    mapQuestionView q =
      Dict.get q.id answers
      |> QuestionView.view q
      |> Html.App.map (Update.AnswerQuestion q.id)
      |> Html.App.map toExplorerMsg
  in
    List.map mapQuestionView questions
