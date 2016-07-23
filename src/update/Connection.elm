module Update.Connection exposing
  ( Msg
    ( DelegateToAssessor
    )
  , update
  , secondaryFetch
  )

import Model.Connection as Connection exposing (Connection)
import Update.Question.Assessor as AssessorUpdate


type alias Tid = Int -- Topic ID


type Msg
  = DelegateToAssessor AssessorUpdate.Msg


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


secondaryFetch : Tid -> Connection -> (Connection, Cmd Msg)
secondaryFetch tid connection =
  let
    (assessor, cmd) =
      AssessorUpdate.init tid <| Connection.key connection
  in
    ( { connection | assessor = assessor }
    , Cmd.map DelegateToAssessor cmd)
