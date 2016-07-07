module Update.Connection exposing
  ( Msg (..)
  , update
  )


import Model.Connection as Connection exposing (Connection)
import Model.Question.Answer exposing (Answer)


import Dict


type alias Qid = Int


type Msg
  = AnswerQuestion Qid Answer


update : Msg -> Connection -> (Connection, Cmd Msg)
update msg connection =
  case msg of
    AnswerQuestion qid answer ->
      { connection | answers = Dict.insert qid answer connection.answers }
      ! []
