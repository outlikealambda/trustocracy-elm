module Update.Question.Answer exposing
  ( Context
  , Msg
    ( Choose )
  , update
  )


import Common.API as API
import Common.Backed as Backed exposing (Backed (Fresh, Linked))


import Model.Question.Answer as Answer exposing (Answer, Choice (None))


type alias Context =
  { tid : Int -- Topic ID
  , oid : Int -- Opinion ID
  , qid : Int -- Question ID
  }


type Msg
  = Choose Answer.Choice
  | WriteComplete (Result String Int)
  | DeleteComplete (Result String Int)


update : Context -> Msg -> Answer -> (Answer, Cmd Msg)
update {tid, oid, qid} msg answer =
  case msg of
    Choose choice ->
      case answer of

        Fresh _ ->
          ( Fresh choice
          , API.createAnswer
            ( WriteComplete << Err )
            ( WriteComplete << Ok)
            choice tid oid qid
          )

        Linked aid _ ->
          ( Linked aid choice
          , saveOrDelete aid choice
          )


    -- comes back with Nothing on delete, the allocated ID on create,
    -- or the existing ID on update
    WriteComplete result ->
      case result of
        Ok aid ->
          ( Backed.Linked aid <| Backed.data answer
          , Cmd.none )
        Err errorMsg ->
          Debug.log ("error saving answer" ++ errorMsg) answer ! []


    DeleteComplete result ->
      case result of
        Ok aid ->
          ( Fresh None
          , Cmd.none )
        Err errorMsg ->
          Debug.log ("error deleting answer" ++ errorMsg) answer ! []


saveOrDelete : Int -> Choice -> Cmd Msg
saveOrDelete aid choice =
  case choice of
    None ->
      API.deleteAnswer
        (DeleteComplete << Err)
        (DeleteComplete << Ok)
        aid
    _ ->
      API.updateAnswer
        ( WriteComplete << Err )
        ( WriteComplete << Ok)
        aid
        choice
