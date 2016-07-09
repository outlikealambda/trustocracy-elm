module Update.Connection exposing
  ( Msg (..)
  , update
  , init
  )

import Common.API as API

import Model.Connection as Connection exposing (Connection)
import Model.Question.Answer as Answer exposing (Answer)


import Dict


type alias QuestionId = Int
type alias TopicId = Int
type alias AnswerId = Int


type Msg
  = AnswerQuestion TopicId QuestionId Answer
  | SaveSuccess QuestionId (Maybe AnswerId)
  | SaveFail QuestionId Answer String
  | FetchedAnswers (List (QuestionId, Answer))
  | Error String


update : Msg -> Connection -> (Connection, Cmd Msg)
update msg connection =
  case msg of
    AnswerQuestion tid qid answer ->
      let
        oid =
          Connection.key connection

        oldAnswer =
          Dict.get qid connection.answers
          |> Maybe.withDefault Answer.unanswered

        onError =
          SaveFail qid oldAnswer

        onSuccess =
          SaveSuccess qid

        cmd =
          case answer.id of
            Nothing ->
              case answer.choice of
                Answer.None ->
                  Cmd.none
                _ ->
                  API.createAnswer onError onSuccess answer tid oid qid
            Just answerId ->
              case answer.choice of
                Answer.None ->
                  API.deleteAnswer onError onSuccess answerId
                _ ->
                  API.updateAnswer onError onSuccess answerId answer

      in
        { connection | answers = Dict.insert qid answer connection.answers }
        ! [ cmd ]

    -- this should only change the ID on create and delete
    SaveSuccess qid answerId ->
      let
        updateAnswerId answer =
          { answer | id = answerId }

        answers =
          Dict.update qid (Maybe.map updateAnswerId) connection.answers

      in
        { connection | answers = answers } ! []

    -- reset the answer to the previous value
    SaveFail qid answer string ->
      { connection | answers = Dict.insert qid answer connection.answers }
      ! []

    FetchedAnswers fetched ->
      { connection | answers = Dict.fromList fetched }
      ! []

    Error string ->
      ( Debug.log ("failed while fetching for connection" ++ string) connection
      , Cmd.none
      )


init : TopicId -> Connection -> (Connection, Cmd Msg)
init tid connection =
  ( connection
  , API.fetchAnswers Error FetchedAnswers tid <| Connection.key connection
  )
