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
          Connection.assessor connection
            |> Maybe.map (AssessorUpdate.update context childMsg)
            |> Maybe.map (Pair.fstMap Just)
            |> Maybe.withDefault ( Nothing, Cmd.none )

      in
        ( Connection.setAssessor assessor connection
        , Cmd.map DelegateToAssessor cmd
        )

    Zoom ->
      Connection.expand connection
        |> zoomFetch context.tid

    FetchedInfluence result ->
      case result of
        Ok influence ->
          Connection.setInfluence (Retrieved influence) connection ! []

        Err errorMsg ->
          let
            e = Debug.log "error fetching influence" errorMsg

          in
            Connection.setInfluence NoRequest connection ! []


zoomFetch : Tid -> Connection -> (Connection, Cmd Msg)
zoomFetch tid connection =
  let
    (assessor, cmd) =
      case Connection.assessor connection of
        Nothing ->
          AssessorUpdate.init tid <| Connection.key connection

        Just loaded ->
          ( loaded, Cmd.none )

  in
    ( Connection.setAssessor (Just assessor) connection
    , Cmd.map DelegateToAssessor cmd)


secondaryFetch : Connection -> (Connection, Cmd Msg)
secondaryFetch connection =
  let
    (influence, influenceCmd) =
      fetchInfluence (Connection.key connection) (Connection.influence connection)

  in
    Connection.setInfluence influence connection
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
