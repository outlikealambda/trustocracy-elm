module Common.API exposing
  ( loginUser
  , checkForActiveUser
  , fetchUserByFacebookAuth
  , fetchUserByGoogleAuth
  , updateGoogleContacts
  , fetchConnectedV2
  , fetchConnectedV3
  , fetchConnectedV4
  , fetchBrowsable
  , fetchInfluence
  , fetchMetrics
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
  , lookupTrustee
  , addPlace
  , fetchPlaces
  , updatePlace
  , removePlace
  )


import Base64
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import String
import Task exposing (Task)
import Time exposing (Time)



import Auth.Facebook as Facebook
import Auth.Google as Google
import Model.SurfacedOpinion.SurfacedOpinion as SurfacedOpinion exposing (SurfacedOpinion)
import Model.Opinion.Composition as Composition exposing (Composition)
import Model.Opinion.Opinion as Opinion exposing (Opinion)
import Model.Opinion.Metrics as Metrics exposing (Metrics)
import Model.Place as Place exposing (Place)
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
  in
    case encodedCredentials of
      Err err ->
        Task.perform onError <| Task.succeed err
      Ok basicAuthCreds ->
        Http.request
          ( httpParams User.decoder
            |> setHeaders
              [ Http.header "Authorization" ("Basic " ++ basicAuthCreds)
              , Http.header "secret" secret
              ]
            |> setUrl (openEndpoint ["login"])
          )
          |> Http.send (responseHandler onError onSuccess)


checkForActiveUser : (String -> a) -> (User -> a) -> Cmd a
checkForActiveUser onError onSuccess =
  Http.get
    (openEndpoint ["checkUser"])
    User.decoder
    |> Http.send (responseHandler onError onSuccess)


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
  Http.request
    ( httpParams User.decoder
      |> setHeaders
        [ Http.header "fbsignedrequest" fbAuthResponse.signedRequest
        , Http.header "fbaccesstoken" fbAuthResponse.accessToken
        ]
      |> setUrl (openEndpoint ["fbUser"])
    )
    |> Http.send (responseHandler onError onSuccess)


fetchUserByGoogleAuth : (String -> a) -> (User -> a) -> Google.AuthResponse -> Cmd a
fetchUserByGoogleAuth onError onSuccess gAuthResponse =
  transmitGoogleAuth (openEndpoint ["gaUser"]) gAuthResponse
    |> Http.send (responseHandler onError onSuccess)


updateGoogleContacts : (String -> a) -> (User -> a) -> Google.AuthResponse -> Cmd a
updateGoogleContacts onError onSuccess gAuthResponse =
  transmitGoogleAuth (openEndpoint ["gaContacts"]) gAuthResponse
    |> Http.send (responseHandler onError onSuccess)


transmitGoogleAuth : Url -> Google.AuthResponse -> Http.Request User
transmitGoogleAuth url gaResponse =
  Http.request
    ( httpParams User.decoder
      |> setHeaders
        [ Http.header "gasignedrequest" gaResponse.idToken
        , Http.header "gaaccesstoken" gaResponse.accessToken
        ]
      |> setUrl url
    )


--------------
-- OPINIONS --
--------------


fetchConnectedV2 : (String -> a) -> (List SurfacedOpinion -> a) -> TopicId -> Cmd a
fetchConnectedV2 onError onSuccess tid =
  fetch
    (secureEndpoint ["topic/", toString tid, "/connected/v2"])
    (Decode.list SurfacedOpinion.connectedDecoder)
    onError
    onSuccess


fetchConnectedV3 : (String -> a) -> (List SurfacedOpinion -> a) -> TopicId -> Cmd a
fetchConnectedV3 onError onSuccess tid =
  fetch
    (secureEndpoint ["topic/", toString tid, "/connected/v3"])
    (Decode.list SurfacedOpinion.connectedDecoder)
    onError
    onSuccess


fetchConnectedV4 : (String -> a) -> (List SurfacedOpinion -> a) -> TopicId -> Cmd a
fetchConnectedV4 onError onSuccess tid =
  fetch
    (secureEndpoint ["topic/", toString tid, "/connected/v4"])
    ( Decode.list
      ( Decode.oneOf
        [ SurfacedOpinion.connectedDecoder
        , SurfacedOpinion.unconnectedDecoder
        ]
      )
    )
    onError
    onSuccess


fetchBrowsable : (String -> a) -> (List SurfacedOpinion -> a) -> TopicId -> Cmd a
fetchBrowsable onError onSuccess topicId =
  fetch
    (openEndpoint ["topic/", toString topicId, "/opinion"])
    (Decode.list SurfacedOpinion.unconnectedDecoder)
    onError
    onSuccess


fetchInfluence : (String -> a) -> (Int -> a) -> OpinionId -> Cmd a
fetchInfluence onError onSuccess oid =
  fetch
    (openEndpoint ["opinion/", toString oid, "/influence"])
    (Decode.field "influence" Decode.int)
    onError
    onSuccess


fetchMetrics : (String -> a) -> (Metrics -> a) -> OpinionId -> Cmd a
fetchMetrics onError onSuccess oid =
  fetch
    (openEndpoint ["opinion/", toString oid, "/metrics"])
    Metrics.decoder
    onError
    onSuccess


fetchOpinionsByTopic : (String -> a) -> (List Opinion -> a) -> TopicId -> Cmd a
fetchOpinionsByTopic onError onSuccess topicId =
  fetch
    (openEndpoint ["topic/", toString topicId, "/opinion"])
    (Decode.list Opinion.decoder)
    onError
    onSuccess


fetchDraftByTopic : (String -> a) -> (Composition -> a) -> TopicId -> Cmd a
fetchDraftByTopic onError onSuccess topicId =
  fetch
    (secureEndpoint ["topic/", toString topicId, "/opinion"])
    Composition.decoder
    onError
    onSuccess


saveOpinion : (String -> a) -> (Composition -> a) -> Composition -> TopicId -> Cmd a
saveOpinion =
  writeOpinion "save"


publishOpinion : (String -> a) -> (Composition -> a) -> Composition -> TopicId -> Cmd a
publishOpinion =
  writeOpinion "publish"


writeOpinion : String -> (String -> a) -> (Composition -> a) -> Composition -> TopicId -> Cmd a
writeOpinion writeType onError onSuccess composition topicId =
  write
    (writeUrlBuilder topicId writeType)
    (Composition.encode composition)
    Composition.decoder
    onError
    onSuccess


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
  fetch
    (openEndpoint ["topic/", toString topicId])
    Topic.decoder
    onError
    onSuccess


fetchAllTopics : (String -> a) -> (List Topic -> a) -> Cmd a
fetchAllTopics onError onSuccess =
  fetch
    (openEndpoint ["topic"])
    (Decode.list Topic.decoder)
    onError
    onSuccess


---------------
-- QUESTIONS --
---------------


fetchQuestions : (String -> a) -> (List Question -> a) -> TopicId -> Cmd a
fetchQuestions onError onSuccess topicId =
  fetch
    (openEndpoint ["topic/", toString topicId, "/question"])
    (Decode.list Question.decoder)
    onError
    onSuccess


fetchAnswers : (String -> a) -> (List (QuestionId, Answer) -> a) -> TopicId -> OpinionId -> Cmd a
fetchAnswers onError onSuccess topicId opinionId =
  fetch
    (secureEndpoint ["topic/", toString topicId, "/opinion/", toString opinionId, "/answer" ])
    (Decode.list Answer.qidPairDecoder)
    onError
    onSuccess

createAnswer : (String -> a) -> (AnswerId -> a) -> Choice -> TopicId -> OpinionId -> QuestionId -> Cmd a
createAnswer onError onSuccess choice tid oid qid =
  let
    url =
      secureEndpoint
          [ "topic/"
          , toString tid
          , "/opinion/"
          , toString oid
          , "/question/"
          , toString qid
          , "/answer"
          ]

  in
    write
      url
      (Answer.encodeChoice choice)
      Answer.idDecoder
      onError
      onSuccess


updateAnswer : (String -> a) -> (AnswerId -> a) -> AnswerId -> Choice -> Cmd a
updateAnswer onError onSuccess answerId choice =
  write
    (secureEndpoint [ "answer/" , toString answerId ])
    (Answer.encodeChoice choice)
    Answer.idDecoder
    onError
    onSuccess


deleteAnswer : (String -> a) -> (AnswerId -> a) -> AnswerId -> Cmd a
deleteAnswer onError onSuccess answerId =
    delete
      (secureEndpoint [ "answer/", toString answerId ])
      (Decode.succeed answerId)
      onError
      onSuccess


--------------
-- TRUSTEES --
--------------


buildTrusteePost : Trustee -> (String -> a) -> (Trustee -> a) -> Cmd a
buildTrusteePost trustee =
  write
    (secureEndpoint ["delegate"])
    (Trustee.encoder trustee)
    Trustee.decoder


setTrustee : (String -> a) -> (Trustee -> a) -> Trustee -> Cmd a
setTrustee onError onSuccess trustee =
  buildTrusteePost trustee onError onSuccess


lookupTrustee : (String -> a) -> (Trustee -> a) -> String -> Cmd a
lookupTrustee onError onSuccess email =
  Http.get
    (secureEndpoint ["delegate/lookup"] ++ "?email=" ++ email)
    Trustee.decoder
    |> Http.send (responseHandler onError onSuccess)

------------
-- PLACES --
------------

addPlace : (String -> a) -> (List Place -> a) -> Cmd a
addPlace onError onSuccess =
  write
    (secureEndpoint ["postLocation"])
    (Place.encoder Place.createEmpty)
    (Decode.list Place.decoder)
    onError
    onSuccess


fetchPlaces : (String -> a) -> (List Place -> a) -> Cmd a
fetchPlaces onError onSuccess =
  fetch
    (secureEndpoint ["getLocation"])
    (Decode.list Place.decoder)
    onError
    onSuccess


updatePlace : (String -> a) -> (Place -> a) -> Place -> Cmd a
updatePlace onError onSuccess place =
  write
    (secureEndpoint ["location/", toString place.id])
    (Place.encoder Place.createEmpty)
    Place.decoder
    onError
    onSuccess


removePlace : (String -> a) -> (Int -> a) -> Int -> Cmd a
removePlace onError onSuccess placeId =
  delete
    (secureEndpoint [ "location/", toString placeId ] )
    Place.removalDecoder
    onError
    onSuccess


fetch : String -> Decode.Decoder b -> (String -> a) -> (b -> a) -> Cmd a
fetch apiPath decoder onError onSuccess =
  Http.get apiPath decoder
    |> Http.send (responseHandler onError onSuccess)


write : String -> Encode.Value -> Decode.Decoder b -> (String -> a) -> (b -> a) -> Cmd a
write apiPath encodedBody decoder onError onSuccess =
  Http.post
    apiPath
    (toJsonBody encodedBody)
    decoder
    |> Http.send (responseHandler onError onSuccess)


delete : String -> Decode.Decoder b -> (String -> a) -> (b -> a) -> Cmd a
delete url decoder onError onSuccess =
  Http.request
    ( httpParams decoder
      |> setMethod "DELETE"
      |> setUrl url
    )
    |> Http.send (responseHandler onError onSuccess)


toJsonBody : Encode.Value -> Http.Body
toJsonBody =
  Http.jsonBody


responseHandler : (String -> a) -> (b -> a) -> Result Http.Error b -> a
responseHandler onError onSuccess result =
  case result of
    Ok value ->
      onSuccess value
    Err error ->
      onError <| httpErrorToString error


httpErrorToString : Http.Error -> String
httpErrorToString err =
  case (Debug.log "HTTP ERROR!" err) of
    Http.BadUrl message ->
      "bad url: " ++ message

    Http.Timeout ->
      "timeout error"

    Http.NetworkError ->
      "network error"

    Http.BadPayload debugMsg response ->
      "json parse error: " ++ debugMsg ++ ", body: " ++ response.body

    Http.BadStatus response ->
      "http response error: " ++ (toString response.status) ++ " " ++ response.body

-------------------------------
-- Request Parameter Helpers --
-------------------------------

type alias RequestParams a =
  { method : String
  , headers : List Http.Header
  , url : String
  , body : Http.Body
  , expect : Http.Expect a
  , timeout : Maybe Time
  , withCredentials : Bool
  }


httpParams : Decode.Decoder a -> RequestParams a
httpParams decoder =
  { method = "GET"
  , headers = []
  , url = ""
  , body = Http.emptyBody
  , expect = Http.expectJson decoder
  , timeout = Nothing
  , withCredentials = False
  }


setMethod : String -> RequestParams a -> RequestParams a
setMethod method rp =
  { rp | method = method }


setHeaders : List Http.Header -> RequestParams a -> RequestParams a
setHeaders headers rp =
  { rp | headers = headers }


setUrl : String -> RequestParams a -> RequestParams a
setUrl url rp =
  { rp | url = url }


setBody : Http.Body -> RequestParams a -> RequestParams a
setBody body rp =
  { rp | body = body }


setExpect : Http.Expect b -> RequestParams a -> RequestParams b
setExpect expect rp =
  { rp | expect = expect }


setWithCredentials : Bool -> RequestParams a -> RequestParams a
setWithCredentials withCredentials rp =
  { rp | withCredentials = withCredentials }
