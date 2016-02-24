module Flat.Opinion
  ( Opinion
  , empty
  , fetchById
  , fetchByUserTopic
  ) where


import Effects exposing (Effects)
import String
import Http
import Task
import Json.Decode as Json exposing ((:=))


type alias Opinion =

  -- from API
  { id : Int
  , text : String
  , influence : Int

  -- derived
  , snippet : String
  , expanded : Bool
  , fetched : Bool
  }


empty : Opinion
empty =
  { id = -1
  , text = ""
  , influence = -1
  , snippet = ""
  , expanded = False
  , fetched = False
  }


fetchById : Int -> Effects Opinion
fetchById opinionId =
  "http://localhost:3714/api/opinion/" ++ (toString opinionId)
    |> Http.get decoder
    |> Task.toMaybe
    |> Task.map (Maybe.withDefault empty)
    |> Effects.task


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


decoder : Json.Decoder Opinion
decoder =
  Json.object3 fromApi
    ( "id" := Json.int )
    ( "text" := Json.string )
    ( "influence" := Json.int )


fromApi : Int -> String -> Int -> Opinion
fromApi id text influence =
  { empty
  | id = id
  , text = text
  , influence = influence
  , fetched = True
  }
