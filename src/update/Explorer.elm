module Update.Explorer exposing
  ( Msg
    ( Focus
    , Blur
    , DelegateToAssessor
    , NextSort
    )
  , init
  , update
  )


import Common.API as API
import Model.TopicOpinion.Connection as Connection exposing (TopicOpinion)
import Model.Explorer as Explorer exposing (Explorer)
import Model.Question.Assessor as Assessor exposing (Assessor)
import Model.Question.Question exposing (Question)


import Update.Question.Assessor as AssessorUpdate
import Update.Connection as ConnectionUpdate


import Utils.Cmd as CmdUtils


import Dict exposing (Dict)


type alias Tid = Int
type alias Oid = Int
type alias Connections = Dict Oid TopicOpinion


type Msg
  = Focus Oid
  | Blur ()
  | DelegateToConnection Oid ConnectionUpdate.Msg
  | DelegateToAssessor Oid AssessorUpdate.Msg
  | FetchedConnections (List TopicOpinion)
  | FetchedQuestions (List Question)
  | NextSort
  | Error String


init : Bool -> Tid -> Maybe Oid -> (Explorer, Cmd Msg)
init isActiveSession tid maybeOid =
  let
    emptyExplorer =
      Explorer.empty

    initMsgs =
      [ fetchConnections isActiveSession tid
      , fetchQuestions tid
      ]

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


fetchConnections : Bool -> Tid -> Cmd Msg
fetchConnections isActiveSession =
  case isActiveSession of
    True ->
      API.fetchConnectedV4 Error FetchedConnections
    False ->
      API.fetchBrowsable Error FetchedConnections


fetchQuestions : Tid -> Cmd Msg
fetchQuestions =
  API.fetchQuestions Error FetchedQuestions


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

    NextSort ->
      Explorer.rotateSort explorer ! []

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


remapPostFetchMessage : (TopicOpinion, Cmd ConnectionUpdate.Msg) -> (TopicOpinion, Cmd Msg)
remapPostFetchMessage pair =
  remapConnectionMsg (Connection.key (fst pair)) pair
