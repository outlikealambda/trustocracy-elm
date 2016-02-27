module Opinion.Opinion
  ( Opinion
  , empty
  , fetchById
  , fetchByUserTopic
  ) where


import User exposing (User)


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
  , user : User

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
  , user = User.empty
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


-- used by the topics page to get browsable opinions
fetchAllByTopic : Int -> Effects (List Opinion)
fetchAllByTopic topicId =
    "http://localhost:3714/api/topic/" ++ (toString topicId) ++ "/opinion"
    |> Http.get (Json.list decoder)
    |> Task.toMaybe
    |> Task.map (Maybe.withDefault [])
    |> Effects.task


-- used by Writer; we don't have the opinionId to load the opinion,
-- we only have the user and topic
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
  Json.object4 fromApi
    ( "id" := Json.int )
    ( "text" := Json.string )
    ( "influence" := Json.int )
    ( "user" := User.decoder )


fromApi : Int -> String -> Int -> User -> Opinion
fromApi id text influence user =
  { empty
  | id = id
  , text = text
  , influence = influence
  , user = user
  , fetched = True
  }
