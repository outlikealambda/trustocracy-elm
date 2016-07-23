module Update.Connection exposing
  ( Msg
    ( AnswerQuestion
    )
  , update
  , init
  )

import Common.API as API


import Model.Connection as Connection exposing (Connection)
import Model.Question.Answer as Answer exposing (Answer)


import Utils.Cmd as CmdUtils


import Dict
import Time exposing (Time)


type alias QuestionId = Int
type alias TopicId = Int
type alias AnswerId = Int


type Msg
  = AnswerQuestion TopicId QuestionId Answer
  | Debounce QuestionId Int Msg
  | SaveAnswer TopicId QuestionId
  | SaveSuccess QuestionId (Maybe AnswerId)
  | SaveFail QuestionId Answer String -- not effective with debounce
  | FetchedAnswers (List (QuestionId, Answer))
  | Error String


debounceWait : Time
debounceWait = 500 * Time.millisecond


update : Msg -> Connection -> (Connection, Cmd Msg)
update msg connection =
  case msg of

    -- VIEW ACCESSIBLE --
    AnswerQuestion tid qid answer ->
      let
        (update, cmds) =
          case answer.choice of
            -- debounce slider movements
            -- don't want to send tons of save requests
            Answer.Rated _ ->
              let
                updateCount =
                  Dict.get qid connection.updateHistory
                    |> Maybe.map ((+) 1)
                    |> Maybe.withDefault 0
                updateHistory =
                  Dict.insert qid updateCount connection.updateHistory
              in
                ( { connection
                  | updateHistory = updateHistory
                  }
                , [ SaveAnswer tid qid
                    |> Debounce qid updateCount
                    |> CmdUtils.delay debounceWait
                  ]
                )

            _ ->
              (connection , [ CmdUtils.init <| SaveAnswer tid qid ])
      in
        { update | answers = Dict.insert qid answer connection.answers }
        ! cmds


    -- INTERNAL ONLY --

    -- TODO: disable input while saving
    SaveAnswer tid qid ->
      let
        oid =
          Connection.key connection

        answer =
          Dict.get qid connection.answers
          |> Maybe.withDefault Answer.unanswered

        onError =
          SaveFail qid answer

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
        connection ! [ cmd ]

    -- don't send this until debounceWait time has passed
    -- since the last Debounce message
    Debounce qid updateCount message ->
      let
        shouldSave =
          Dict.get qid connection.updateHistory
            |> Maybe.map ((==) updateCount)
            |> Maybe.withDefault False
        cmd =
          if shouldSave then
            CmdUtils.init message
          else
            Cmd.none
      in
        connection ! [cmd]


    -- this should only change the ID on create and delete
    SaveSuccess qid answerId ->
      let
        updateAnswerId answer =
          { answer | id = answerId }

        answers =
          Dict.update qid (Maybe.map updateAnswerId) connection.answers

      in
        { connection | answers = answers } ! []


    -- Reset the answer to the previous value
    --
    -- TODO: use a savedState to reset the connection, as is
    -- it doesn't work with debounce
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
