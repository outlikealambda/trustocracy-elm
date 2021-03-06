module Update.Question.Assessor exposing
  ( Context
  , Msg
    (DelegateToAnswer)
  , update
  , load
  )


import Common.API as API


import Model.Question.Answer as Answer exposing (Answer)
import Model.Question.Assessor as Assessor exposing (Assessor)


import Update.Question.Answer as AnswerUpdate


import Dict


type alias Tid = Int -- Topic ID
type alias Oid = Int -- Opinion ID
type alias Qid = Int -- Question ID


type alias Context =
  { tid : Tid
  , oid : Oid
  }


type Msg
  = DelegateToAnswer Qid AnswerUpdate.Msg
  | FetchSuccess (List (Qid, Answer))
  | FetchFail String


update : Context -> Msg -> Assessor -> (Assessor, Cmd Msg)
update {tid, oid} message assessor =
  case assessor of

    Assessor.Disabled ->
      (assessor, Cmd.none)

    Assessor.Enabled answers ->
      let (updated, cmds) =
        case message of
          DelegateToAnswer qid answerMsg ->
            let
              context =
                { tid = tid
                , oid = oid
                , qid = qid
                }
              (updated, command) =
                Dict.get qid answers
                |> Maybe.withDefault Answer.unanswered
                |> AnswerUpdate.update context answerMsg

            in
              Dict.insert qid updated answers
              ! [ Cmd.map (DelegateToAnswer qid) command ]

          FetchSuccess fetchedPairs ->
            Dict.fromList fetchedPairs ! []

          FetchFail error ->
            Debug.log ("failed fetching answers with error: " ++ error) answers
            ! []

      in
        Assessor.Enabled updated
        ! [cmds]


load : Context -> Assessor -> (Assessor, Cmd Msg)
load {tid, oid} assessor =
  case assessor of
    Assessor.Disabled ->
      (Assessor.Disabled, Cmd.none)

    Assessor.Enabled _ ->
      ( Assessor.Enabled Dict.empty
      , API.fetchAnswers FetchFail FetchSuccess tid oid
      )
