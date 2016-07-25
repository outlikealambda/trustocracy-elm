module Model.Question.Answer exposing
  ( Answer
  , Choice
    ( None
    , Picked
    , Rated
    )
  , encodeChoice
  , qidPairDecoder
  , idDecoder
  , unanswered
  )


import Common.Tether as Tether exposing (Tether)


import Json.Decode as Decode exposing ((:=))
import Json.Encode as Encode


type alias Answer = Tether Int Choice


type Choice
  = None
  | Picked Int
  | Rated Float


unanswered : Answer
unanswered = Tether.init None


encodeChoice : Choice -> Encode.Value
encodeChoice choice =
  case choice of
    Picked choiceId ->
      Encode.object [ ("picked", Encode.int choiceId) ]
    Rated rating ->
      Encode.object [ ("rated", Encode.float rating) ]
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
fromApi aid choice =
  Tether.attach aid <| Tether.init choice


idDecoder : Decode.Decoder Int
idDecoder =
  ( "id" := Decode.int )
