module Test.Opinion
  ( Opinion
  , empty
  , setText
  , fetchByUserTopic
  ) where

import Effects exposing (Effects)
import Http
import Task
import String
import Json.Decode as Json exposing ((:=))

type alias Opinion =
  { id : Int
  , text : String
  , influence : Int
  , fetched : Bool
  }


empty : Opinion
empty =
  { id = -1
  , text = ""
  , influence = 0
  , fetched = False
  }


decoder : Json.Decoder Opinion
decoder =
  Json.object3 fromApi
    ( "id" := Json.int )
    ( "influence" := Json.int )
    ( "text" := Json.string )


fromApi : Int -> Int -> String -> Opinion
fromApi id influence text =
  { id = id
  , influence = influence
  , text = text
  , fetched = True
  }


setText : Opinion -> String -> Opinion
setText opinion text =
  { opinion | text = text }


fetchByUserTopic : Int -> Int -> Effects Opinion
fetchByUserTopic userId topicId =
  buildFetchByUserTopicUrl userId topicId
    |> Http.get decoder
    |> Task.toMaybe
    |> Task.map (Maybe.withDefault empty)
    |> Effects.task


buildFetchByUserTopicUrl : Int -> Int -> String
buildFetchByUserTopicUrl userId topicId =
  String.concat
    [ "http://localhost:3714/api/user/"
    , toString userId
    , "/topic/"
    , toString topicId
    , "/opinion"
    ]
