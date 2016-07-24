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

import Utils.Pair as Pair


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

    DelegateToAssessor childMsg ->
      let
        context =
          { tid = context.tid
          , oid = Connection.key connection
          }
        (assessor, cmd) =
          connection.assessor
            |> Maybe.map (AssessorUpdate.update context childMsg)
            |> Maybe.map (Pair.fstMap Just)
            |> Maybe.withDefault ( Nothing, Cmd.none )
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
      case connection.assessor of
        Nothing ->
          AssessorUpdate.init tid <| Connection.key connection
        Just loaded ->
          ( loaded, Cmd.none )

  in
    ( { connection | assessor = Just assessor }
    , Cmd.map DelegateToAssessor cmd)
