module Common.API
  ( loginUser
  , checkForActiveUser
  , fetchUserByFacebookAuth
  , fetchUserByGoogleAuth
  , fetchConnected
  , fetchOpinionById
  , fetchIdsByTopic
  , fetchDraftByTopic
  , saveOpinion
  , publishOpinion
  , fetchTopic
  , fetchAllTopics
  ) where


import Effects exposing (Effects)
import Http
import Json.Decode as Decode exposing ((:=))
import Json.Encode as Encode
import String
import Task


import Auth.Facebook as Facebook
import Auth.Google as Google
import Opinion.Path as Path exposing (Path)
import Opinion.Opinion as Opinion exposing (Opinion)
import User exposing (User)
import Topic.Model as Topic exposing (Topic)


rootUrl : String
rootUrl = "http://localhost:3714/api/"


openEndpoint : List String -> String
openEndpoint =
  (++) rootUrl << String.concat


secureEndpoint : List String -> String
secureEndpoint =
  (++) (rootUrl ++ "secure/") << String.concat


loginUser : (String, String) -> (Maybe User -> a) -> Effects a
loginUser (name, secret) transform =
  Http.send Http.defaultSettings
    { verb = "GET"
    , headers =
      [ ("name", name)
      , ("secret", secret)
      ]
    , url = openEndpoint ["login"]
    , body = Http.empty
    }
    |> Http.fromJson User.decoder
    |> Task.toMaybe
    |> Task.map transform
    |> Effects.task


checkForActiveUser : (Maybe User -> a) -> Effects a
checkForActiveUser transform =
  Http.send Http.defaultSettings
    { verb = "GET"
    , headers = []
    , url = openEndpoint ["checkUser"]
    , body = Http.empty
    }
    |> Http.fromJson User.decoder
    |> Task.toMaybe
    |> Task.map transform
    |> Effects.task


{- send signedRequest string as a header
   server needs to:
   1. split on .
   2. base64url decode each half
   3. check signature (first half) using:
       - declared algorithm
       - payload (second half)
       - our app secret (stored on server)
-}
fetchUserByFacebookAuth : (Maybe User -> a) -> Facebook.AuthResponse -> Effects a
fetchUserByFacebookAuth transform fbAuthResponse =
  Http.send Http.defaultSettings
    { verb = "GET"
    , headers =
      [ ("fbsignedrequest", fbAuthResponse.signedRequest)
      , ("fbaccesstoken", fbAuthResponse.accessToken)
      ]
    , url = openEndpoint ["fbUser"]
    , body = Http.empty
    }
  |> Http.fromJson User.decoder
  |> Task.toMaybe
  |> Task.map transform
  |> Effects.task


fetchUserByGoogleAuth : (Maybe User -> a) -> Google.AuthResponse -> Effects a
fetchUserByGoogleAuth transform gaResponse =
  Http.send Http.defaultSettings
    { verb = "GET"
    , headers =
      [ ("gasignedrequest", gaResponse.idToken)
      ]
    , url =  openEndpoint ["gaUser"]
    , body = Http.empty
    }
  |> Http.fromJson User.decoder
  |> Task.toMaybe
  |> Task.map transform
  |> Effects.task


fetchConnected : (Maybe (List Path) -> a) -> Topic -> Effects a
fetchConnected transform topic =
  secureEndpoint ["topic/", toString topic.id, "/connected"]
    |> Http.get ("paths" := Decode.list Path.decoder)
    |> Task.toMaybe
    |> Task.map transform
    |> Effects.task


fetchOpinionById : (Maybe Opinion -> a) -> Int -> Effects a
fetchOpinionById transform opinionId =
  openEndpoint ["opinion/", toString opinionId]
    |> Http.get Opinion.decoder
    |> Task.toMaybe
    |> Task.map transform
    |> Effects.task


fetchOpinionsByTopic : (Maybe (List Opinion) -> a) -> Int -> Effects a
fetchOpinionsByTopic transform topicId =
  openEndpoint ["topic/", toString topicId, "/opinion"]
    |> Http.get (Decode.list Opinion.decoder)
    |> Task.toMaybe
    |> Task.map transform
    |> Effects.task


-- super inefficient; gets all the opinions, and then extracts the id
-- TODO: new endpoint on server
fetchIdsByTopic : (Maybe (List Int) -> a) -> Topic -> Effects a
fetchIdsByTopic transform topic =
  fetchOpinionsByTopic
    (transform << Maybe.map (List.map .id))
    topic.id


fetchDraftByTopic : (Maybe Opinion -> a) -> Int -> Effects a
fetchDraftByTopic transform topicId =
  secureEndpoint ["topic/", toString topicId, "/opinion"]
    |> Http.get Opinion.decoder
    |> Task.toMaybe
    |> Task.map transform
    |> Effects.task


saveOpinion : (Maybe Opinion -> a) -> Opinion -> Int -> Effects a
saveOpinion =
  writeOpinion "save"


publishOpinion : (Maybe Opinion -> a) -> Opinion -> Int -> Effects a
publishOpinion =
  writeOpinion "publish"


writeOpinion : String -> (Maybe Opinion -> a) -> Opinion -> Int -> Effects a
writeOpinion writeType transform opinion topicId =
  Opinion.encode opinion
    |> Encode.encode 0 -- no pretty print
    |> Http.string
    |> post' Opinion.decoder (writeUrlBuilder topicId writeType)
    |> Task.toMaybe
    |> Task.map transform
    |> Effects.task


writeUrlBuilder : Int -> String -> String
writeUrlBuilder topicId writeType =
  secureEndpoint
    [ "topic/"
    , toString topicId
    , "/opinion/"
    , writeType
    ]

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

------------
-- TOPICS --
------------

fetchTopic : (Maybe Topic -> a) -> Int -> Effects a
fetchTopic transform topicId =
  openEndpoint ["topic/", toString topicId]
    |> Http.get Topic.decoder
    |> Task.toMaybe
    |> Task.map transform
    |> Effects.task


fetchAllTopics : (Maybe (List Topic) -> a) -> Effects a
fetchAllTopics transform =
  openEndpoint ["topic"]
    |> Http.get (Decode.list Topic.decoder)
    |> Task.toMaybe
    |> Task.map transform
    |> Effects.task
