module Model.Question.Assessor exposing
  ( Assessor
    ( Enabled
    , Disabled
    )
  , empty
  , clear
  )


import Model.Question.Answer exposing (Answer)


import Dict exposing (Dict)


type alias Qid = Int -- Question ID
type alias Answers = Dict Qid Answer


type Assessor
  = Enabled Answers
  | Disabled


empty : Bool -> Assessor
empty isActiveSession =
  case isActiveSession of
    True ->
      Enabled Dict.empty

    False ->
      Disabled


clear : Assessor -> Assessor
clear assessor =
  case assessor of
    Enabled _ ->
      Enabled Dict.empty
    Disabled ->
      Disabled
