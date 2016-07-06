module Update.Connection exposing
  ( Msg (..)
  , update
  )


import Model.Connection as Connection exposing (Connection)
import Update.Question.Question as QuestionUpdate


import Dict


type alias Qid = Int


type Msg
  = QuestionMsg Qid QuestionUpdate.Msg


update : Msg -> Connection -> (Connection, Cmd Msg)
update msg connection =
  case msg of
    QuestionMsg qid questionMsg ->
      let
        goUpdate (update, updateCmd) =
          { connection | questions = Dict.insert qid update connection.questions }
          ! [ Cmd.map (QuestionMsg qid) updateCmd ]
      in
        Dict.get qid connection.questions
        |> Maybe.map (QuestionUpdate.update questionMsg)
        |> Maybe.map goUpdate
        |> Maybe.withDefault (connection, Cmd.none)
