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
import Model.Extend.Writeable as Writeable exposing (Writeable)


import Json.Decode as Decode exposing ((:=))
import Json.Encode as Encode


type alias Answer = Createable ( Writeable { choice : Choice } )


type Choice
  = None
  | Picked Int
  | Rated Float


unanswered : Answer
unanswered =
  { id = Nothing
  , choice = None
  , writeStatus = Writeable.None
  }


encode : Answer -> Encode.Value
encode answer =
  case answer.choice of
    Picked choiceId ->
      Encode.object [ ("picked", Encode.int choiceId) ]
    Rated rating ->
      Encode.object [ ("rated", Encode.float rating) ]
      -- Encode.string (Debug.crash "We don't support rating answers yet")
    None ->
      Encode.string (Debug.crash "We shouldn't be encoding None answers")


decoder : Decode.Decoder Answer
decoder =
  Decode.object2 fromApi
    ( "id" := Decode.int )
    choiceDecoder


choiceDecoder : Decode.Decoder Choice
choiceDecoder =
  Decode.oneOf
    [ "picked" := Decode.map Picked Decode.int
    , "rated" := Decode.map Rated Decode.float
    ]


qidPairDecoder : Decode.Decoder (Int, Answer)
qidPairDecoder =
  Decode.object2 (,)
    ("questionId" := Decode.int)
    decoder


fromApi : Int -> Choice -> Answer
fromApi id choice =
  { id = Just id
  , choice = choice
  , writeStatus = Writeable.None
  }


idDecoder : Decode.Decoder Int
idDecoder =
  ( "id" := Decode.int )
