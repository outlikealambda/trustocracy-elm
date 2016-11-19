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
import Model.SurfacedOpinion.SurfacedOpinion as SurfacedOpinion exposing (SurfacedOpinion)
import Model.Explorer as Explorer exposing (Explorer)
import Model.Question.Assessor as Assessor exposing (Assessor)
import Model.Question.Question exposing (Question)


import Update.Question.Assessor as AssessorUpdate
import Update.SurfacedOpinion as SurfacedOpinionUpdate


import Utils.Cmd as CmdUtils


import Dict exposing (Dict)


type alias Tid = Int
type alias Oid = Int
type alias SurfacedOpinions = Dict Oid SurfacedOpinion


type Msg
  = Focus Oid
  | Blur ()
  | DelegateToSurfacedOpinion Oid SurfacedOpinionUpdate.Msg
  | DelegateToAssessor Oid AssessorUpdate.Msg
  | FetchedSurfacedOpinions (List SurfacedOpinion)
  | FetchedQuestions (List Question)
  | NextSort
  | Error String


init : Bool -> Tid -> Maybe Oid -> (Explorer, Cmd Msg)
init isActiveSession tid maybeOid =
  let
    emptyExplorer =
      Explorer.empty

    initMsgs =
      [ fetchSurfacedOpinions isActiveSession tid
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


fetchSurfacedOpinions : Bool -> Tid -> Cmd Msg
fetchSurfacedOpinions isActiveSession =
  case isActiveSession of
    True ->
      API.fetchConnectedV4 Error FetchedSurfacedOpinions
    False ->
      API.fetchBrowsable Error FetchedSurfacedOpinions


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

    DelegateToSurfacedOpinion cId msg ->
      delegateSurfacedOpinionMsg context cId msg explorer

    FetchedSurfacedOpinions fetched ->
      let
        (surfacedOpinions, cmds) =
          List.map SurfacedOpinionUpdate.secondaryFetch fetched
            |> List.map remapPostFetchMessage
            |> List.unzip
      in
        { explorer | surfacedOpinions = SurfacedOpinion.toDict surfacedOpinions }
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


delegateSurfacedOpinionMsg : Context -> Oid -> SurfacedOpinionUpdate.Msg -> Explorer -> (Explorer, Cmd Msg)
delegateSurfacedOpinionMsg context cId msg explorer =
  let
    goUpdate (update, updateCmd) =
      { explorer | surfacedOpinions = Dict.insert cId update explorer.surfacedOpinions }
      ! [ updateCmd ]
  in
    Dict.get cId explorer.surfacedOpinions
    |> Maybe.map (SurfacedOpinionUpdate.update context msg)
    |> Maybe.map (remapSurfacedOpinionMsg cId)
    |> Maybe.map goUpdate
    |> Maybe.withDefault (explorer, Cmd.none)


remapSurfacedOpinionMsg : Int -> (c, Cmd SurfacedOpinionUpdate.Msg) -> (c, Cmd Msg)
remapSurfacedOpinionMsg = CmdUtils.mapCmdPair << DelegateToSurfacedOpinion


remapPostFetchMessage : (SurfacedOpinion, Cmd SurfacedOpinionUpdate.Msg) -> (SurfacedOpinion, Cmd Msg)
remapPostFetchMessage pair =
  remapSurfacedOpinionMsg (SurfacedOpinion.key (fst pair)) pair
