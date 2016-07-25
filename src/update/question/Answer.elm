module Update.Question.Answer exposing
  ( Context
  , Msg
    ( Choose )
  , update
  )


import Common.API as API
import Common.Tether as Tether exposing (Tether)


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
update context msg answer =
  case msg of
    Choose choice ->
      ( Tether.map (\_ -> choice) answer
      , Tether.id answer
          |> Maybe.map (saveOrDelete choice)
          |> Maybe.withDefault (createOrNothing choice context)
      )


    -- comes back with Nothing on delete, the allocated ID on create,
    -- or the existing ID on update
    WriteComplete result ->
      case result of
        Ok aid ->
          ( Tether.attach aid answer
          , Cmd.none )
        Err errorMsg ->
          Debug.log ("error saving answer" ++ errorMsg) answer ! []


    DeleteComplete result ->
      case result of
        Ok aid ->
          ( Tether.detach answer
          , Cmd.none )
        Err errorMsg ->
          Debug.log ("error deleting answer" ++ errorMsg) answer ! []


saveOrDelete : Choice -> Int -> Cmd Msg
saveOrDelete choice aid =
  case choice of
    None ->
      API.deleteAnswer
        (DeleteComplete << Err)
        (DeleteComplete << Ok)
        aid
    _ ->
      API.updateAnswer
        (WriteComplete << Err)
        (WriteComplete << Ok)
        aid
        choice


createOrNothing : Choice -> Context -> Cmd Msg
createOrNothing choice {tid, oid, qid} =
  case choice of
    None ->
      Cmd.none
    _ ->
      API.createAnswer
        (WriteComplete << Err)
        (WriteComplete << Ok)
        choice tid oid qid
