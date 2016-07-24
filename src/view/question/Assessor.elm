module View.Question.Assessor exposing
  ( questions
  )


import Model.Question.Assessor exposing (Assessor)
import Model.Question.Question as Question exposing (Question)

import Update.Question.Assessor as AssessorUpdate

import View.Question.Question as QuestionView


import Dict exposing (Dict)
import Html exposing (Html)
import Html.App
import Html.Attributes exposing (class)


type alias Qid = Int -- Question ID


questions : List Question -> Maybe Assessor -> Html AssessorUpdate.Msg
questions questions mAssessor =

  case mAssessor of
    Nothing ->
      Html.div [] [ Html.text "Loading..." ]
    Just assessor ->
      Html.div
        [ class "questions" ]
        ( List.map (mapQuestion assessor) questions )


mapQuestion : Assessor -> Question -> Html AssessorUpdate.Msg
mapQuestion {answers} q =
  Dict.get q.id answers
  |> QuestionView.view q
  |> Html.App.map (AssessorUpdate.DelegateToAnswer q.id)
