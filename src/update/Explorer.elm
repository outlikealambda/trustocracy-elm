module Update.Explorer exposing
  ( Msg
    ( Focus
    , Blur
    )
  , init
  , update
  )


import Common.API as API
import Model.Connection exposing (Connection)
import Model.Expandable as Expandable
import Model.Explorer as Explorer exposing (Explorer)


import Dict


type alias TopicId = Int
type alias OpinionId = Int


type Msg
  = Focus OpinionId
  | Blur ()
  | FetchComplete (List Connection)
  | Error String


init : TopicId -> Maybe OpinionId -> (Explorer, Cmd Msg)
init tid maybeOid =
  let
    zoom =
      Maybe.map Explorer.Focused maybeOid
        |> Maybe.withDefault Explorer.Blurred
  in
    ( { connections = Dict.empty
      , zoom = zoom
      }
    , API.fetchConnectedV2 Error FetchComplete tid
    )


update : Msg -> Explorer -> (Explorer, Cmd Msg)
update message explorer =
  case message of
    Focus key ->
      let
        blurred =
          Dict.map (\_ -> Expandable.collapse) explorer.connections
        focused =
          Dict.update key (Maybe.map Expandable.expand) blurred
      in
        { explorer | connections = focused } ! []

    Blur () ->
      let blurred =
        Dict.map (\_ -> Expandable.collapse) explorer.connections
      in
        { explorer | connections = blurred } ! []

    FetchComplete fetched ->
      let
        connections =
          List.map (\connection -> (connection.opinion.id, connection)) fetched
            |> Dict.fromList
      in
        { explorer | connections = connections } ! []

    Error err ->
      let
        msg = Debug.log "error in Surveyer!" err
      in
        explorer ! []
