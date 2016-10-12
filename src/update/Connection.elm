module Update.Connection exposing
  ( Msg
    ( DelegateToAssessor
    , Zoom
    )
  , update
  , secondaryFetch
  )

import Common.API as API
import Common.Remote as Remote exposing
  ( Remote
    ( NoRequest
    , Requested
    , Retrieved
    )
  )

import Model.Connection as Connection exposing (Connection)
import Model.Extend.Expandable as Expandable
import Update.Question.Assessor as AssessorUpdate

import Utils.Pair as Pair


type alias Tid = Int -- Topic ID

type Msg
  = DelegateToAssessor AssessorUpdate.Msg
  | Zoom
  | FetchedInfluence (Result String Int)


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
        |> zoomFetch context.tid

    FetchedInfluence result ->
      case result of
        Ok influence ->
          { connection | influence = Retrieved influence } ! []

        Err errorMsg ->
          let
            e = Debug.log "error fetching influence" errorMsg

          in
            { connection | influence = NoRequest } ! []


zoomFetch : Tid -> Connection -> (Connection, Cmd Msg)
zoomFetch tid connection =
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


secondaryFetch : Connection -> (Connection, Cmd Msg)
secondaryFetch connection =
  let
    (influence, influenceCmd) =
      fetchInfluence (Connection.key connection) connection.influence

  in
    { connection | influence = influence }
    ! [ influenceCmd ]


fetchInfluence : Int -> Remote Int -> (Remote Int, Cmd Msg)
fetchInfluence key influence =
  case influence of
    NoRequest ->
      ( Requested
      , API.fetchInfluence
        (FetchedInfluence << Err)
        (FetchedInfluence << Ok)
        key
      )
    _ ->
      ( influence
      , Cmd.none
      )
