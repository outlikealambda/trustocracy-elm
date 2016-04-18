module Opinion.Opinion
  ( Opinion
  , empty
  , fetchById
  , fetchDraftByUserTopic
  , fetchAllByTopic
  , save
  , publish
  ) where


import User exposing (User)
import Trustee exposing (Trustee)
import Qualifications as Qualifications exposing (Qualifications)


import Effects exposing (Effects)
import String
import Http
import Task
import Json.Decode as Decode exposing ((:=))
import Json.Encode as Encode


type alias Opinion =

  -- from API
  { id : Int
  , text : String
  , influence : Int
  , opiner : Trustee
  , qualifications : Qualifications

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
  , opiner = Trustee.empty
  , qualifications = Qualifications.empty
  , snippet = ""
  , expanded = False
  , fetched = False
  }


fetchById : Int -> (Opinion, Effects Opinion)
fetchById opinionId =
  ( { empty | id = opinionId }
  , "http://localhost:3714/api/opinion/" ++ (toString opinionId)
    |> Http.get decoder
    |> Task.toMaybe
    |> Task.map (Maybe.withDefault empty)
    |> Effects.task
  )


-- used by the topics page to get browsable opinions
fetchAllByTopic : Int -> Effects (List Opinion)
fetchAllByTopic topicId =
    "http://localhost:3714/api/topic/" ++ (toString topicId) ++ "/opinion"
    |> Http.get (Decode.list decoder)
    |> Task.toMaybe
    |> Task.map (Maybe.withDefault [])
    |> Effects.task


-- used by Writer; we don't have the opinionId to load the opinion,
-- we only have the user and topic
fetchByUserTopic : (Int -> Int -> String) -> User -> Int -> Effects Opinion
fetchByUserTopic buildUrl user topicId =
  buildUrl user.id topicId
    |> Http.get decoder
    |> Task.toMaybe
    |> Task.map
      ( Maybe.withDefault
        { empty
        | opiner = Trustee.fromSelf user.name user.id
        , fetched = True
        }
      )
    |> Effects.task


fetchDraftByUserTopic : User -> Int -> Effects Opinion
fetchDraftByUserTopic = fetchByUserTopic buildFetchDraftByUserTopicUrl


save : Opinion -> Int -> Effects (Maybe Opinion)
save opinion topicId =
  write (Debug.log "saving" opinion) <| buildWriteUrl opinion topicId "save"


publish : Opinion -> Int -> Effects (Maybe Opinion)
publish opinion topicId =
  write opinion <| buildWriteUrl opinion topicId "publish"


write : Opinion -> String -> Effects (Maybe Opinion)
write opinion url =
  encode opinion
    |> Encode.encode 0
    |> Http.string
    |> post' decoder url
    |> Task.toMaybe
    |> Effects.task


buildWriteUrl : Opinion -> Int -> String -> String
buildWriteUrl {opiner} topicId writeType =
  String.concat
    [ "http://localhost:3714/api/user/"
    , toString opiner.id
    , "/topic/"
    , toString topicId
    , "/opinion/"
    , writeType
    ]


buildFetchDraftByUserTopicUrl : Int -> Int -> String
buildFetchDraftByUserTopicUrl userId topicId =
  String.concat
    [ "http://localhost:3714/api/user/"
    , toString userId
    , "/topic/"
    , toString topicId
    , "/opinion"
    ]


decoder : Decode.Decoder Opinion
decoder =
  Decode.object5 fromApi
    ( "id" := Decode.int )
    ( "text" := Decode.string )
    ( "influence" := Decode.int )
    ( "opiner" := Trustee.decoder )
    ( Decode.oneOf
      [ "qualifications" := Qualifications.decoder
      , Decode.succeed Qualifications.empty
      ]
    )


encode : Opinion -> Encode.Value
encode opinion =
  Encode.object
    [ ("id", Encode.int opinion.id)
    , ("text", Encode.string opinion.text)
    , ("influence", Encode.int opinion.influence)
    , ("opiner", Trustee.encoder opinion.opiner)
    , ("qualifications", Qualifications.encode opinion.qualifications)
    ]


fromApi : Int -> String -> Int -> Trustee -> Qualifications -> Opinion
fromApi id text influence opiner qualifications =
  { empty
  | id = id
  , text = text
  , influence = influence
  , opiner = opiner
  , qualifications = qualifications
  , fetched = True
  }


-- because post is pretty worthless
-- see: https://groups.google.com/forum/#!topic/elm-discuss/Zpq9itvtLEY
post' : Decode.Decoder a -> String -> Http.Body -> Task.Task Http.Error a
post' decoder url body =
    Http.send Http.defaultSettings
      { verb = "POST"
      , headers = [("Content-type", "application/json")]
      , url = url
      , body = body
      }
    |> Http.fromJson decoder
