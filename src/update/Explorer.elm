module Update.Explorer exposing
  ( Msg
    ( Focus
    , Blur
    , ConnectionMsg
    )
  , initConnected
  , initAll
  , update
  )


import Common.API as API
import Model.Connection.Connection as Connection exposing (Connection)
import Model.Explorer as Explorer exposing (Explorer)
import Model.Question.Question exposing (Question)


import Update.Connection as ConnectionUpdate


import Dict exposing (Dict)


type alias Tid = Int
type alias Oid = Int
type alias Connections = Dict Oid Connection


type Msg
  = Focus Oid ConnectionUpdate.Msg
  | Blur ()
  | ConnectionMsg Oid ConnectionUpdate.Msg
  | FetchedConnections (List Connection)
  | FetchedQuestions (List Question)
  | Error String


initConnected : Tid -> Maybe Oid -> (Explorer, Cmd Msg)
initConnected =
  init << fetchConnected


initAll : Tid -> Maybe Oid -> (Explorer, Cmd Msg)
initAll =
  init << fetchAll


init : List (Cmd Msg) -> Maybe Oid -> (Explorer, Cmd Msg)
init cmds maybeOid =
  let
    zoom =
      Maybe.map Explorer.Focused maybeOid
        |> Maybe.withDefault Explorer.Blurred
  in
    { connections = Dict.empty
    , zoom = zoom
    , questions = [] -- need to fetch questions here
    }
    ! cmds


fetchConnected : Tid -> List (Cmd Msg)
fetchConnected tid =
  [ API.fetchConnectedV3 Error FetchedConnections tid
  , API.fetchQuestions Error FetchedQuestions tid
  ]


fetchAll : Tid -> List (Cmd Msg)
fetchAll tid =
  [ API.fetchBrowsable Error FetchedConnections tid
  , API.fetchQuestions Error FetchedQuestions tid
  ]


type alias Context =
  { tid : Tid
  }


update : Context -> Msg -> Explorer -> (Explorer, Cmd Msg)
update context message explorer =
  case message of
    -- blurs all connections before passing through the message
    -- to the identified connection
    Focus cId childMsg ->
      { explorer | connections = blurAll explorer.connections }
        |> delegateConnectionMsg context cId childMsg

    Blur () ->
      { explorer | connections = blurAll explorer.connections } ! []

    ConnectionMsg cId msg ->
      delegateConnectionMsg context cId msg explorer

    FetchedConnections fetched ->
      let
        (connections, cmds) =
          List.map ConnectionUpdate.secondaryFetch fetched
            |> List.map remapPostFetchMessage
            |> List.unzip
      in
        { explorer | connections = Connection.toDict connections }
        ! cmds

    FetchedQuestions questions ->
      { explorer | questions = questions } ! []

    Error err ->
      let
        msg = Debug.log "error in Surveyer!" err
      in
        explorer ! []


blurAll : Connections -> Connections
blurAll connections =
  Dict.map (\_ -> Connection.collapse) connections


delegateConnectionMsg : Context -> Oid -> ConnectionUpdate.Msg -> Explorer -> (Explorer, Cmd Msg)
delegateConnectionMsg context cId msg explorer =
  let
    goUpdate (update, updateCmd) =
      { explorer | connections = Dict.insert cId update explorer.connections }
      ! [ updateCmd ]
  in
    Dict.get cId explorer.connections
    |> Maybe.map (ConnectionUpdate.update context msg)
    |> Maybe.map (remapConnectionMsg cId)
    |> Maybe.map goUpdate
    |> Maybe.withDefault (explorer, Cmd.none)


remapConnectionMsg : Int -> (c, Cmd ConnectionUpdate.Msg) -> (c, Cmd Msg)
remapConnectionMsg cId (c, connectionMsg) =
  (c, Cmd.map (ConnectionMsg cId) connectionMsg)


remapPostFetchMessage : (Connection, Cmd ConnectionUpdate.Msg) -> (Connection, Cmd Msg)
remapPostFetchMessage pair =
  remapConnectionMsg (Connection.key (fst pair)) pair
