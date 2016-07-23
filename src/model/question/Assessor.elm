module Model.Question.Assessor exposing
  ( Assessor
  , empty
  )


import Model.Question.Answer exposing (Answer)


import Dict exposing (Dict)


type alias Qid = Int -- Question ID


type alias Assessor =
  { answers : Dict Qid Answer
  }


empty : Assessor
empty =
  { answers = Dict.empty
  }
