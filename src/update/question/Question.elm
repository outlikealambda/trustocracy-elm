module Update.Question.Question exposing
  ( Msg (..)
  , update
  )


import Model.Question.Chosen exposing (Chosen)
import Model.Question.Question as Question exposing (Question)


type Msg
  = ChosenMsg Chosen


update : Msg -> Question -> (Question, Cmd Msg)
update msg question =
  case msg of
    ChosenMsg chosen ->
      { question | chosen = chosen } ! []
