module Update.Connection exposing
  ( Msg
    ( DelegateToAssessor
    , Zoom
    )
  , update
  )

import Model.Connection as Connection exposing (Connection)
import Model.Extend.Expandable as Expandable
import Update.Question.Assessor as AssessorUpdate


type alias Tid = Int -- Topic ID


type Msg
  = DelegateToAssessor AssessorUpdate.Msg
  | Zoom


type alias Context =
  { tid : Tid
  }


update : Context -> Msg -> Connection -> (Connection, Cmd Msg)
update context msg connection =
  case msg of

    DelegateToAssessor assessorMsg ->
      let
        context =
          { tid = context.tid
          , oid = Connection.key connection
          }
        (assessor, cmd) =
          AssessorUpdate.update context assessorMsg connection.assessor
      in
        ( { connection | assessor = assessor }
        , Cmd.map DelegateToAssessor cmd
        )

    Zoom ->
      Expandable.expand connection
        |> secondaryFetch context.tid


secondaryFetch : Tid -> Connection -> (Connection, Cmd Msg)
secondaryFetch tid connection =
  let
    (assessor, cmd) =
      AssessorUpdate.init tid <| Connection.key connection
  in
    ( { connection | assessor = assessor }
    , Cmd.map DelegateToAssessor cmd)
