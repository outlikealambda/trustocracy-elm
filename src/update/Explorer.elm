module Update.Explorer exposing
  ( Msg
    ( Focus
    , Blur
    , DelegateToAssessor
    )
  , init
  , update
  )


import Common.API as API
import Model.Connection.Connection as Connection exposing (Connection)
import Model.Explorer as Explorer exposing (Explorer)
import Model.Question.Assessor as Assessor exposing (Assessor)
import Model.Question.Question exposing (Question)


import Update.Question.Assessor as AssessorUpdate
import Update.Connection as ConnectionUpdate


import Utils.Cmd as CmdUtils


import Dict exposing (Dict)


type alias Tid = Int
type alias Oid = Int
type alias Connections = Dict Oid Connection


type Msg
  = Focus Oid
  | Blur ()
  | DelegateToConnection Oid ConnectionUpdate.Msg
  | DelegateToAssessor Oid AssessorUpdate.Msg
  | FetchedConnections (List Connection)
  | FetchedQuestions (List Question)
  | Error String


init : Bool -> Tid -> Maybe Oid -> (Explorer, Cmd Msg)
init isActiveSession tid maybeOid =
  let
    emptyExplorer =
      Explorer.empty

    initMsgs =
      fetchConnected tid

    maybeContext =
      Maybe.map (\oid -> {tid = tid, oid = oid}) maybeOid

    emptyAssessor =
      Assessor.empty isActiveSession

    (assessor, assessorMsg) =
      case maybeOid of
        Nothing ->
          emptyAssessor ! []

        Just oid ->
          AssessorUpdate.load {tid = tid, oid = oid} emptyAssessor
            |> CmdUtils.mapCmdPair (DelegateToAssessor oid)

    zoom =
      Maybe.map Explorer.Focused maybeOid
        |> Maybe.withDefault Explorer.Blurred

  in
    { emptyExplorer
    | zoom = zoom
    , assessor = assessor
    }
    ! (assessorMsg :: initMsgs)


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
    Focus oid ->
      let
        assessorContext =
          { tid = context.tid
          , oid = oid
          }

        (assessor, assessorMsg) =
          AssessorUpdate.load assessorContext explorer.assessor
            |> CmdUtils.mapCmdPair (DelegateToAssessor oid)

      in
        { explorer
        | assessor = assessor
        , zoom = Explorer.Focused oid
        } ! [ assessorMsg ]

    Blur () ->
      { explorer
      | assessor = Assessor.clear explorer.assessor
      , zoom = Explorer.Blurred
      }
      ! []

    DelegateToConnection cId msg ->
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

    DelegateToAssessor oid msg ->
      let
        assessorContext =
          { tid = context.tid
          , oid = oid
          }

        (assessor, assessorMsg) =
          AssessorUpdate.update assessorContext msg explorer.assessor
            |> CmdUtils.mapCmdPair (DelegateToAssessor oid)

      in
        { explorer | assessor = assessor }
        ! [ assessorMsg ]

    Error err ->
      let
        msg = Debug.log "error in Surveyer!" err
      in
        explorer ! []


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
remapConnectionMsg = CmdUtils.mapCmdPair << DelegateToConnection


remapPostFetchMessage : (Connection, Cmd ConnectionUpdate.Msg) -> (Connection, Cmd Msg)
remapPostFetchMessage pair =
  remapConnectionMsg (Connection.key (fst pair)) pair
