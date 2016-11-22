module Model.Topic exposing
  ( Topic
  , empty
  , decoder
  )


import Date exposing (Date)
import Json.Decode as Decode
import Time


type alias Topic =
  { id : Int
  , text : String
  , isComplete : Bool
  , opinionCount : Int
  , created : Date
  , lastUpdated : Date
  }

empty : Topic
empty =
  { id = -1
  , text = ""
  , isComplete = False
  , opinionCount = 0
  , created = Date.fromTime 0
  , lastUpdated = Date.fromTime 0
  }


decoder : Decode.Decoder Topic
decoder =
  Decode.map5 fromApi
  (Decode.field "id" Decode.int)
  (Decode.field "text" Decode.string)
  (Decode.field "opinionCount" Decode.int)
  (Decode.field "created" <|
    Decode.oneOf
      [ Decode.null Nothing
      , Decode.map Just Decode.float
      ]
  )
  (Decode.field "lastUpdated" <|
    Decode.oneOf
      [ Decode.null Nothing
      , Decode.map Just Decode.float
      ]
  )


fromApi : Int -> String -> Int -> Maybe Float -> Maybe Float -> Topic
fromApi id text opinionCount created lastUpdated=
  { id = id
  , text = text
  , isComplete = True
  , opinionCount = opinionCount
  , created =
    Date.fromTime <| Time.second * (Maybe.withDefault 0 created)
  , lastUpdated =
    Date.fromTime <| Time.second * (Maybe.withDefault 0 lastUpdated)
  }
