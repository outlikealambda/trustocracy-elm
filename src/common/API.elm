module Common.API exposing
  ( loginUser
  , checkForActiveUser
  , fetchUserByFacebookAuth
  , fetchUserByGoogleAuth
  , updateGoogleContacts
  , fetchConnected
  , fetchConnectedV2
  , fetchConnectedV3
  , fetchBrowsable
  , fetchInfluence
  , fetchDraftByTopic
  , saveOpinion
  , publishOpinion
  , fetchTopic
  , fetchAllTopics
  , fetchQuestions
  , fetchAnswers
  , createAnswer
  , updateAnswer
  , deleteAnswer
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
import Model.Connection as Connection exposing (Connection)
import Model.Opinion.Composition as Composition exposing (Composition)
import Model.Opinion.Opinion as Opinion exposing (Opinion)
import Model.Path as Path exposing (Path)
import Model.Question.Answer as Answer exposing (Answer, Choice)
import Model.Question.Question as Question exposing (Question)
import Model.Topic as Topic exposing (Topic)
import Model.Trustee as Trustee exposing (Trustee)
import Model.User as User exposing (User)


type alias Url = String
type alias TopicId = Int
type alias OpinionId = Int
type alias QuestionId = Int
type alias AnswerId = Int


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
loginUser onError onSuccess (name, secret) =
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
    Task.perform onError onSuccess task


checkForActiveUser : (String -> a) -> (User -> a) -> Cmd a
checkForActiveUser onError onSuccess =
  Http.send Http.defaultSettings
    { verb = "GET"
    , headers = []
    , url = openEndpoint ["checkUser"]
    , body = Http.empty
    }
    |> Http.fromJson User.decoder
    |> Task.mapError httpErrorToString
    |> Task.perform onError onSuccess


{- send signedRequest string as a header
   server needs to:
   1. split on .
   2. base64url decode each half
   3. check signature (first half) using:
       - declared algorithm
       - payload (second half)
       - our app secret (stored on server)
-}
fetchUserByFacebookAuth : (String -> a) -> (User -> a) -> Facebook.AuthResponse -> Cmd a
fetchUserByFacebookAuth onError onSuccess fbAuthResponse =
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
  |> Task.mapError httpErrorToString
  |> Task.perform onError onSuccess


fetchUserByGoogleAuth : (String -> a) -> (User -> a) -> Google.AuthResponse -> Cmd a
fetchUserByGoogleAuth onError onSuccess gAuthResponse =
  transmitGoogleAuth (openEndpoint ["gaUser"])
    >> Task.perform onError onSuccess
    <| gAuthResponse


updateGoogleContacts : (String -> a) -> (User -> a) -> Google.AuthResponse -> Cmd a
updateGoogleContacts onError onSuccess gAuthResponse =
  transmitGoogleAuth (secureEndpoint ["gaContacts"])
    >> Task.perform onError onSuccess
    <| gAuthResponse


transmitGoogleAuth : Url -> Google.AuthResponse -> Task String User
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
  |> Task.mapError httpErrorToString


--------------
-- OPINIONS --
--------------


fetchConnected : (String -> a) -> (List Path -> a) -> Topic -> Cmd a
fetchConnected onError onSuccess topic =
  secureEndpoint ["topic/", toString topic.id, "/connected"]
    |> Http.get ("paths" := Decode.list Path.decoder)
    |> Task.mapError httpErrorToString
    |> Task.perform onError onSuccess


fetchConnectedV2 : (String -> a) -> (List Connection -> a) -> TopicId -> Cmd a
fetchConnectedV2 onError onSuccess tid =
  secureEndpoint ["topic/", toString tid, "/connected/v2"]
    |> Http.get (Decode.list Connection.decoder)
    |> Task.mapError httpErrorToString
    |> Task.perform onError onSuccess


fetchConnectedV3 : (String -> a) -> (List Connection -> a) -> TopicId -> Cmd a
fetchConnectedV3 onError onSuccess tid =
  secureEndpoint ["topic/", toString tid, "/connected/v3"]
    |> Http.get (Decode.list Connection.decoder)
    |> Task.mapError httpErrorToString
    |> Task.perform onError onSuccess


fetchBrowsable : (String -> a) -> (List Connection -> a) -> TopicId -> Cmd a
fetchBrowsable onError onSuccess topicId =
  openEndpoint ["topic/", toString topicId, "/opinion"]
    |> Http.get (Decode.list Connection.noPathDecoder)
    |> Task.mapError httpErrorToString
    |> Task.perform onError onSuccess

fetchInfluence : (String -> a) -> (Int -> a) -> OpinionId -> Cmd a
fetchInfluence onError onSuccess oid =
  openEndpoint ["opinion/", toString oid, "/influence"]
    |> Http.get ("influence" := Decode.int)
    |> Task.mapError httpErrorToString
    |> Task.perform onError onSuccess


fetchOpinionsByTopic : (String -> a) -> (List Opinion -> a) -> TopicId -> Cmd a
fetchOpinionsByTopic onError onSuccess topicId =
  openEndpoint ["topic/", toString topicId, "/opinion"]
    |> Http.get (Decode.list Opinion.decoder)
    |> Task.mapError httpErrorToString
    |> Task.perform onError onSuccess


fetchDraftByTopic : (String -> a) -> (Composition -> a) -> TopicId -> Cmd a
fetchDraftByTopic onError onComplete topicId =
  secureEndpoint ["topic/", toString topicId, "/opinion"]
    |> Http.get Composition.decoder
    |> Task.mapError httpErrorToString
    |> Task.perform onError onComplete


saveOpinion : (String -> a) -> (Composition -> a) -> Composition -> TopicId -> Cmd a
saveOpinion =
  writeOpinion "save"


publishOpinion : (String -> a) -> (Composition -> a) -> Composition -> TopicId -> Cmd a
publishOpinion =
  writeOpinion "publish"


writeOpinion : String -> (String -> a) -> (Composition -> a) -> Composition -> TopicId -> Cmd a
writeOpinion writeType onError onSuccess composition topicId =
  Composition.encode composition
    |> Encode.encode 0 -- no pretty print
    |> Http.string
    |> post' Composition.decoder (writeUrlBuilder topicId writeType)
    |> Task.mapError httpErrorToString
    |> Task.perform onError onSuccess


writeUrlBuilder : TopicId -> String -> String
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


fetchTopic : (String -> a) -> (Topic -> a) -> TopicId -> Cmd a
fetchTopic onError onSuccess topicId =
  openEndpoint ["topic/", toString topicId]
    |> Http.get Topic.decoder
    |> Task.mapError httpErrorToString
    |> Task.perform onError onSuccess


fetchAllTopics : (String -> a) -> (List Topic -> a) -> Cmd a
fetchAllTopics onError onComplete =
  openEndpoint ["topic"]
    |> Http.get (Decode.list Topic.decoder)
    |> Task.mapError httpErrorToString
    |> Task.perform onError onComplete


---------------
-- QUESTIONS --
---------------


fetchQuestions : (String -> a) -> (List Question -> a) -> TopicId -> Cmd a
fetchQuestions onError onSuccess topicId =
  openEndpoint ["topic/", toString topicId, "/question"]
    |> Http.get (Decode.list Question.decoder)
    |> Task.mapError httpErrorToString
    |> Task.perform onError onSuccess


fetchAnswers : (String -> a) -> (List (QuestionId, Answer) -> a) -> TopicId -> OpinionId -> Cmd a
fetchAnswers onError onSuccess topicId opinionId =
  secureEndpoint ["topic/", toString topicId, "/opinion/", toString opinionId, "/answer" ]
    |> Http.get (Decode.list Answer.qidPairDecoder)
    |> Task.mapError httpErrorToString
    |> Task.perform onError onSuccess


createAnswer : (String -> a) -> (AnswerId -> a) -> Choice -> TopicId -> OpinionId -> QuestionId -> Cmd a
createAnswer onError onSuccess choice tid oid qid =
  let
    postAnswer =
      post' Answer.idDecoder
        <| secureEndpoint
          [ "topic/"
          , toString tid
          , "/opinion/"
          , toString oid
          , "/question/"
          , toString qid
          , "/answer"
          ]
  in
    Answer.encodeChoice choice
      |> Encode.encode 0
      |> Http.string
      |> postAnswer
      |> Task.mapError httpErrorToString
      |> Task.perform onError onSuccess


updateAnswer : (String -> a) -> (AnswerId -> a) -> AnswerId -> Choice -> Cmd a
updateAnswer onError onSuccess answerId choice =
  let
    putAnswer =
      post' Answer.idDecoder
        <| secureEndpoint
          [ "answer/"
          , toString answerId
          ]
  in
    Answer.encodeChoice choice
      |> Encode.encode 0
      |> Http.string
      |> putAnswer
      |> Task.mapError httpErrorToString
      |> Task.perform onError onSuccess


deleteAnswer : (String -> a) -> (AnswerId -> a) -> AnswerId -> Cmd a
deleteAnswer onError onSuccess answerId =
  secureEndpoint [ "answer/", toString answerId ]
    |> delete (Decode.succeed answerId)
    |> Task.mapError httpErrorToString
    |> Task.perform onError onSuccess


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


setTrustee : (String -> a) -> (Trustee -> a) -> Trustee -> Cmd a
setTrustee onError onSuccess trustee =
  setTrusteeTask trustee
    |> Task.mapError httpErrorToString
    |> Task.perform onError onSuccess


setTrustees : (List Trustee -> a) -> List Trustee -> Cmd a
setTrustees transform trustees =
  List.map setTrusteeTask trustees
    |> List.map Task.toResult
    |> Task.sequence
    |> Task.map (List.filterMap Result.toMaybe)
    |> Task.perform (\_ -> transform []) transform


lookupTrustee : (String -> a) -> (Trustee -> a) -> String -> Cmd a
lookupTrustee onError onSuccess email =
  Http.url (secureEndpoint ["delegate/lookup"]) [ ("email", email) ]
    |> Http.get Trustee.decoder
    |> Task.mapError httpErrorToString
    |> Task.perform onError onSuccess


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


delete : Decode.Decoder a -> String -> Task.Task Http.Error a
delete decoder url =
  Http.send Http.defaultSettings
    { verb = "DELETE"
    , headers = [("Content-type", "plain/text")]
    , url = url
    , body = Http.empty
    }
  |> Http.fromJson decoder


httpErrorToString : Http.Error -> String
httpErrorToString err =
  case (Debug.log "HTTP ERROR!" err) of
    Http.Timeout ->
      "timeout error"

    Http.NetworkError ->
      "network error"

    Http.UnexpectedPayload msg ->
      "json parse error: " ++ msg

    Http.BadResponse code msg ->
      "http response error: " ++ (toString code) ++ " " ++ msg
