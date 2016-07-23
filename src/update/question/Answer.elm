module Update.Question.Answer exposing
  ( Context
  , Msg
    ( Choose )
  , update
  )


import Common.API as API


import Model.Extend.Writeable as Writeable
import Model.Question.Answer as Answer exposing (Answer)


type alias Context =
  { tid : Int -- Topic ID
  , oid : Int -- Opinion ID
  , qid : Int -- Question ID
  }


type Msg
  = Choose Answer.Choice
  | WriteSuccess (Maybe Int)
  | WriteFail String


type CRUD
  = Create
  | Update Int
  | Delete Int
  | None


update : Context -> Msg -> Answer -> (Answer, Cmd Msg)
update {tid, oid, qid} msg answer =
  case msg of
    Choose choice ->
      let
        updated =
          { answer
          | choice = choice
          , writeStatus = Writeable.Writing
          }

        cmd =
          case extractCrudAction updated of
            Create ->
              API.createAnswer WriteFail WriteSuccess updated tid oid qid

            Update answerId ->
              API.updateAnswer WriteFail WriteSuccess answerId updated

            Delete answerId ->
              API.deleteAnswer WriteFail WriteSuccess answerId

            None ->
              Cmd.none

      in
        ( updated, cmd )

    -- comes back with Nothing on delete, the allocated ID on create,
    -- or the existing ID on update
    WriteSuccess maybeAnswerId ->
      ( { answer
        | id = maybeAnswerId
        , writeStatus = Writeable.Written
        }
      , Cmd.none
      )

    WriteFail string ->
      { answer | writeStatus = Writeable.Failed } ! []


extractCrudAction : Answer -> CRUD
extractCrudAction {id, choice} =
  case id of
    Nothing ->
      case choice of
        Answer.None ->
          None

        _ ->
          Create

    Just allocatedId ->
      case choice of
        Answer.None ->
          Delete allocatedId

        _ ->
          Update allocatedId
