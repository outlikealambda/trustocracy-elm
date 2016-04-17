module Common.API where

import Effects exposing (Effects)
import Http
import String
import Task

import User exposing (User)
import Auth.Facebook as Facebook
import Auth.Google as Google


fetchUser : (String, String) -> (Maybe User -> a) -> Effects a
fetchUser (name, secret) transform =
  Http.send Http.defaultSettings
    { verb = "GET"
    , headers =
      [ ("name", name)
      , ("secret", secret)
      ]
    , url = "http://localhost:3714/api/login"
    , body = Http.empty
    }
    |> Http.fromJson User.decoder
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


fetchUserByGoogleAuth : (Maybe User -> a) -> Google.AuthResponse -> Effects a
fetchUserByGoogleAuth transform gaResponse =
  Http.send Http.defaultSettings
    { verb = "GET"
    , headers =
      [ ("gasignedrequest", gaResponse.idToken)
      ]
    , url = "http://localhost:3714/api/gaUser"
    , body = Http.empty
    }
  |> Http.fromJson User.decoder
  |> Task.toMaybe
  |> Task.map transform
  |> Effects.task
