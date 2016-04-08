module Common.API where

import Effects exposing (Effects)
import Http
import String
import Task

import User exposing (User)
import Auth.Facebook as Facebook

fetchUser : Int -> (Maybe User -> a) -> Effects a
fetchUser userId transform =
  buildUserUrl userId
    |> Http.get User.decoder
    |> Task.toMaybe
    |> Task.map transform
    |> Effects.task

buildUserUrl : Int -> String
buildUserUrl userId =
  String.concat
    [ "http://localhost:3714/api/user/"
    , toString userId
    ]

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
    , url = "http://localhost:3714/api/fbUser"
    , body = Http.empty
    }
  |> Http.fromJson User.decoder
  |> Task.toMaybe
  |> Task.map transform
  |> Effects.task
