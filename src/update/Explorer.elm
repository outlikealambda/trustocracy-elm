module Update.Explorer exposing
  ( Msg
    ( Focus
    , Blur
    , ConnectionMsg
    )
  , init
  , update
  )


import Common.API as API
import Model.Connection as Connection exposing (Connection)
import Model.Extend.Expandable as Expandable
import Model.Explorer as Explorer exposing (Explorer)
import Model.Question.Question exposing (Question)


import Update.Connection as ConnectionUpdate


import Dict


type alias Tid = Int
type alias Oid = Int


type Msg
  = Focus Oid
  | Blur ()
  | ConnectionMsg Oid ConnectionUpdate.Msg
  | FetchedConnections (List Connection)
  | FetchedQuestions (List Question)
  | Error String


init : Tid -> Maybe Oid -> (Explorer, Cmd Msg)
init tid maybeOid =
  let
    zoom =
      Maybe.map Explorer.Focused maybeOid
        |> Maybe.withDefault Explorer.Blurred
  in
    { connections = Dict.empty
    , zoom = zoom
    , questions = [] -- need to fetch questions here
    }
    ! [ API.fetchConnectedV3 Error FetchedConnections tid
      , API.fetchQuestions Error FetchedQuestions tid
      ]


type alias Context =
  { tid : Tid
  }


update : Context -> Msg -> Explorer -> (Explorer, Cmd Msg)
update context message explorer =
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

    ConnectionMsg cId msg ->
      let
        goUpdate (update, updateCmd) =
          { explorer | connections = Dict.insert cId update explorer.connections }
          ! [ updateCmd ]
      in
        Dict.get cId explorer.connections
        |> Maybe.map (ConnectionUpdate.update context msg)
        |> Maybe.map remapConnectionMsg
        |> Maybe.map goUpdate
        |> Maybe.withDefault (explorer, Cmd.none)

    FetchedConnections fetched ->
      let
        mergeModel (connections, commands) =
          { explorer | connections = Connection.toDict connections }
          ! commands
      in
        List.map (ConnectionUpdate.secondaryFetch context.tid) fetched
        |> List.map remapConnectionMsg
        |> List.unzip
        |> mergeModel

    FetchedQuestions fetched ->
      { explorer | questions = fetched } ! []

    Error err ->
      let
        msg = Debug.log "error in Surveyer!" err
      in
        explorer ! []


remapConnectionMsg : (Connection, Cmd ConnectionUpdate.Msg) -> (Connection, Cmd Msg)
remapConnectionMsg (connection, connectionMsg) =
  (connection, Cmd.map (ConnectionMsg connection.opinion.id) connectionMsg)
