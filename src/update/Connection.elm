module Update.Connection exposing
  ( Msg
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

import Model.Connection.Connection as Connection exposing (Connection)



type alias Tid = Int -- Topic ID

type Msg
  = FetchedInfluence (Result String Int)


type alias Context =
  { tid : Tid
  }


update : Context -> Msg -> Connection -> (Connection, Cmd Msg)
update context msg connection =
  case msg of

    FetchedInfluence result ->
      case result of
        Ok influence ->
          Connection.setInfluence (Retrieved influence) connection ! []

        Err errorMsg ->
          let
            e = Debug.log "error fetching influence" errorMsg

          in
            Connection.setInfluence NoRequest connection ! []


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
