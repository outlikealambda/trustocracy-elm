module Model.Question.Answer exposing
  ( Answer
  , Choice
    ( None
    , Picked
    , Rated
    )
  , encode
  , qidPairDecoder
  , idDecoder
  , unanswered
  )


import Model.Extend.Createable exposing (Createable)


import Json.Decode as Decode exposing ((:=))
import Json.Encode as Encode


type alias Answer = Createable {choice : Choice}


type Choice
  = None
  | Picked Int
  | Rated (List (Int, Float))


unanswered : Answer
unanswered =
  { id = Nothing
  , choice = None
  }


encode : Answer -> Encode.Value
encode answer =
  case answer.choice of
    Picked choiceId ->
      Encode.object [ ("pickOne", Encode.int choiceId) ]
    Rated _ ->
      Encode.string (Debug.log "Uh oh" "We don't support rating answers yet")
    None ->
      Encode.string (Debug.log "Uh oh" "We shouldn't be encoding None answers")


decoder : Decode.Decoder Answer
decoder =
  Decode.object2 fromApi
    ( "id" := Decode.int )
    choiceDecoder


choiceDecoder : Decode.Decoder Choice
choiceDecoder =
  Decode.oneOf
    [ "pickOne" := Decode.map Picked Decode.int
    , "rated" := Decode.map Rated (Decode.list rating)
    ]


rating : Decode.Decoder (Int, Float)
rating =
  Decode.object2 (,)
    ( "id" := Decode.int )
    ( "rating" := Decode.float )


qidPairDecoder : Decode.Decoder (Int, Answer)
qidPairDecoder =
  Decode.object2 (,)
    ("questionId" := Decode.int)
    decoder


fromApi : Int -> Choice -> Answer
fromApi id choice =
  { id = Just id
  , choice = choice
  }


idDecoder : Decode.Decoder Int
idDecoder =
  ( "id" := Decode.int )
