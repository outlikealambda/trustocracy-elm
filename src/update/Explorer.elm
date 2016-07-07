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
import Model.Connection exposing (Connection)
import Model.Expandable as Expandable
import Model.Explorer as Explorer exposing (Explorer)
import Model.Question.Question exposing (Question)


import Update.Connection as ConnectionUpdate


import Dict


type alias TopicId = Int
type alias OpinionId = Int


type Msg
  = Focus OpinionId
  | Blur ()
  | ConnectionMsg OpinionId ConnectionUpdate.Msg
  | FetchedConnections (List Connection)
  | FetchedQuestions (List Question)
  | Error String


init : TopicId -> Maybe OpinionId -> (Explorer, Cmd Msg)
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
      , API.fetchTopicQuestions Error FetchedQuestions tid
      ]


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

    ConnectionMsg cId msg ->
      let
        goUpdate (update, updateCmd) =
          { explorer | connections = Dict.insert cId update explorer.connections }
          ! [ Cmd.map (ConnectionMsg cId) updateCmd ]
      in
        Dict.get cId explorer.connections
        |> Maybe.map (ConnectionUpdate.update msg)
        |> Maybe.map goUpdate
        |> Maybe.withDefault (explorer, Cmd.none)

    FetchedConnections fetched ->
      let
        connections =
          List.map (\connection -> (connection.opinion.id, connection)) fetched
            |> Dict.fromList
      in
        { explorer | connections = connections } ! []

    FetchedQuestions fetched ->
      { explorer | questions = fetched } ! []

    Error err ->
      let
        msg = Debug.log "error in Surveyer!" err
      in
        explorer ! []
