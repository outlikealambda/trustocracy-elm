module Common.API exposing
  ( loginUser
  , checkForActiveUser
  , fetchUserByFacebookAuth
  , fetchUserByGoogleAuth
  , updateGoogleContacts
  , fetchConnected
  , fetchOpinionById
  , fetchIdsByTopic
  , fetchDraftByTopic
  , saveOpinion
  , publishOpinion
  , fetchTopic
  , fetchAllTopics
  , setTrustee
  , setTrustees
  , lookupTrustee
  )


import Base64
import Http
import Json.Decode as Decode exposing ((:=))
import Json.Encode as Encode
import String
import Task exposing (Task)


import Auth.Facebook as Facebook
import Auth.Google as Google
import Opinion.Path as Path exposing (Path)
import Opinion.Opinion as Opinion exposing (Opinion)
import User exposing (User)
import Topic.Model as Topic exposing (Topic)
import Trustee exposing (Trustee)


type alias Url = String


rootUrl : Url
rootUrl = "http://localhost:3714/api/"


openEndpoint : List String -> Url
openEndpoint =
  (++) rootUrl << String.concat


secureEndpoint : List String -> Url
secureEndpoint =
  (++) (rootUrl ++ "secure/") << String.concat


----------
-- USER --
----------


loginUser : (String -> a) -> (User -> a) -> (String, String) -> Cmd a
loginUser errTransform userTransform (name, secret) =
  let
    encodedCredentials =
      Base64.encode <| (name ++ ":" ++ secret)
    task =
      case encodedCredentials of
        Err err ->
          Task.fail err
        Ok basicAuthCreds ->
          Http.send Http.defaultSettings
            { verb = "GET"
            , headers =
              [ ("Authorization", "Basic " ++ basicAuthCreds)
              , ("secret", secret)
              ]
            , url = openEndpoint ["login"]
            , body = Http.empty
            }
            |> Http.fromJson User.decoder
            |> Task.mapError httpErrorToString
  in
    Task.perform errTransform userTransform task


httpErrorToString : Http.Error -> String
httpErrorToString err =
  case err of
    Http.Timeout ->
      "timeout error"

    Http.NetworkError ->
      "network error"

    Http.UnexpectedPayload msg ->
      "json parse error: " ++ msg

    Http.BadResponse code msg ->
      "http response error: " ++ (toString code) ++ " " ++ msg


checkForActiveUser : (Maybe User -> a) -> Cmd a
checkForActiveUser transform =
  Http.send Http.defaultSettings
    { verb = "GET"
    , headers = []
    , url = openEndpoint ["checkUser"]
    , body = Http.empty
    }
    |> Http.fromJson User.decoder
    |> Task.toMaybe
    |> Task.perform (\_ -> transform Nothing) transform


{- send signedRequest string as a header
   server needs to:
   1. split on .
   2. base64url decode each half
   3. check signature (first half) using:
       - declared algorithm
       - payload (second half)
       - our app secret (stored on server)
-}
fetchUserByFacebookAuth : (Maybe User -> a) -> Facebook.AuthResponse -> Cmd a
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
  |> Task.perform (\_ -> transform Nothing) transform


fetchUserByGoogleAuth : (Maybe User -> a) -> Google.AuthResponse -> Cmd a
fetchUserByGoogleAuth transform =
  transmitGoogleAuth (openEndpoint ["gaUser"])
    >> Task.perform (\_ -> transform Nothing) transform


updateGoogleContacts : (Maybe User -> a) -> Google.AuthResponse -> Cmd a
updateGoogleContacts transform =
  transmitGoogleAuth (secureEndpoint ["gaContacts"])
    >> Task.perform (\_ -> transform Nothing) transform


transmitGoogleAuth : Url -> Google.AuthResponse -> Task x (Maybe User)
transmitGoogleAuth url gaResponse =
  Http.send Http.defaultSettings
    { verb = "GET"
    , headers =
      [ ("gasignedrequest", gaResponse.idToken)
      , ("gaaccesstoken", gaResponse.accessToken)
      ]
    , url = url
    , body = Http.empty
    }
  |> Http.fromJson User.decoder
  |> Task.toMaybe


--------------
-- OPINIONS --
--------------


fetchConnected : (Maybe (List Path) -> a) -> Topic -> Cmd a
fetchConnected transform topic =
  secureEndpoint ["topic/", toString topic.id, "/connected"]
    |> Http.get ("paths" := Decode.list Path.decoder)
    |> Task.toMaybe
    |> Task.perform (\_ -> transform Nothing) transform


fetchOpinionById : (Maybe Opinion -> a) -> Int -> Cmd a
fetchOpinionById transform opinionId =
  openEndpoint ["opinion/", toString opinionId]
    |> Http.get Opinion.decoder
    |> Task.toMaybe
    |> Task.perform (\_ -> transform Nothing) (transform)


fetchOpinionsByTopic : (Maybe (List Opinion) -> a) -> Int -> Cmd a
fetchOpinionsByTopic transform topicId =
  openEndpoint ["topic/", toString topicId, "/opinion"]
    |> Http.get (Decode.list Opinion.decoder)
    |> Task.toMaybe
    |> Task.perform (\_ -> transform Nothing) transform


-- super inefficient; gets all the opinions, and then extracts the id
-- TODO: new endpoint on server
fetchIdsByTopic : (Maybe (List Int) -> a) -> Topic -> Cmd a
fetchIdsByTopic transform topic =
  fetchOpinionsByTopic
    (transform << Maybe.map (List.map .id))
    topic.id


fetchDraftByTopic : (Maybe Opinion -> a) -> Int -> Cmd a
fetchDraftByTopic transform topicId =
  secureEndpoint ["topic/", toString topicId, "/opinion"]
    |> Http.get Opinion.decoder
    |> Task.toMaybe
    |> Task.perform (\_ -> transform Nothing) transform


saveOpinion : (Maybe Opinion -> a) -> Opinion -> Int -> Cmd a
saveOpinion =
  writeOpinion "save"


publishOpinion : (Maybe Opinion -> a) -> Opinion -> Int -> Cmd a
publishOpinion =
  writeOpinion "publish"


writeOpinion : String -> (Maybe Opinion -> a) -> Opinion -> Int -> Cmd a
writeOpinion writeType transform opinion topicId =
  Opinion.encode opinion
    |> Encode.encode 0 -- no pretty print
    |> Http.string
    |> post' Opinion.decoder (writeUrlBuilder topicId writeType)
    |> Task.toMaybe
    |> Task.perform (\_ -> transform Nothing) transform


writeUrlBuilder : Int -> String -> String
writeUrlBuilder topicId writeType =
  secureEndpoint
    [ "topic/"
    , toString topicId
    , "/opinion/"
    , writeType
    ]


------------
-- TOPICS --
------------


fetchTopic : (Maybe Topic -> a) -> Int -> Cmd a
fetchTopic transform topicId =
  openEndpoint ["topic/", toString topicId]
    |> Http.get Topic.decoder
    |> Task.toMaybe
    |> Task.perform (\_ -> transform Nothing) transform


fetchAllTopics : (Maybe (List Topic) -> a) -> Cmd a
fetchAllTopics transform =
  openEndpoint ["topic"]
    |> Http.get (Decode.list Topic.decoder)
    |> Task.toMaybe
    |> Task.perform (\_ -> transform Nothing) transform


--------------
-- TRUSTEES --
--------------


setTrusteeTask : Trustee -> Task Http.Error Trustee
setTrusteeTask trustee =
  Trustee.encoder trustee
    |> Encode.encode 0
    |> Http.string
    |> post'
      Trustee.decoder
      (secureEndpoint ["delegate"])


setTrustee : (Maybe Trustee -> a) -> Trustee -> Cmd a
setTrustee transform trustee =
  setTrusteeTask trustee
    |> Task.toMaybe
    |> Task.perform (\_ -> transform Nothing) transform


setTrustees : (List Trustee -> a) -> List Trustee -> Cmd a
setTrustees transform trustees =
  List.map setTrusteeTask trustees
    |> List.map Task.toResult
    |> Task.sequence
    |> Task.map (List.filterMap Result.toMaybe)
    |> Task.perform (\_ -> transform []) transform


lookupTrustee : (Maybe Trustee -> a) -> String -> Cmd a
lookupTrustee transform email =
  Http.url (secureEndpoint ["delegate/lookup"]) [ ("email", email) ]
    |> Http.get Trustee.decoder
    |> Task.toMaybe
    |> Task.perform (\_ -> transform Nothing) transform


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
