module View.Question.Assessor exposing
  ( questions
  )


import Model.Question.Answer exposing (Answer)
import Model.Question.Assessor as Assessor exposing (Assessor)
import Model.Question.Question as Question exposing (Question)

import Update.Question.Assessor as AssessorUpdate

import View.Question.Question as QuestionView


import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes exposing (class)


type alias Qid = Int -- Question ID


questions : List Question -> Assessor -> Html AssessorUpdate.Msg
questions questions mAssessor =
  case mAssessor of

    Assessor.Disabled ->
      Html.div [ class "assessor-disabled" ] []

    Assessor.Enabled answers ->
      Html.div
        [ class "questions" ]
        ( List.map (mapQuestion answers) questions )


mapQuestion : Dict Int Answer -> Question -> Html AssessorUpdate.Msg
mapQuestion answers q =
  Dict.get q.id answers
  |> QuestionView.view q
  |> Html.map (AssessorUpdate.DelegateToAnswer q.id)
