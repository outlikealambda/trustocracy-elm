module Update.Connection exposing
  ( Msg (..)
  , update
  )


import Model.Connection as Connection exposing (Connection)
import Model.Question.Chosen exposing (Chosen)


import Dict


type alias Qid = Int


type Msg
  = AnswerQuestion Qid Chosen


update : Msg -> Connection -> (Connection, Cmd Msg)
update msg connection =
  case msg of
    AnswerQuestion qid chosen ->
      { connection | answers = Dict.insert qid chosen connection.answers }
      ! []
