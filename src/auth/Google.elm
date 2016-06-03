port module Auth.Google exposing
  ( AuthResponse
  , login
  , logout
  , requestContacts
  , authResponses
  )


type alias AuthResponse =
  { idToken: String
  , expiresAt: String
  , accessToken: String
  , scope: String
  }


port googleAuthResponses : (Maybe AuthResponse -> msg) -> Sub msg

authResponses : (Maybe AuthResponse -> msg) -> Sub msg
authResponses = googleAuthResponses


port googleLogin : () -> Cmd msg

login : Cmd msg
login = googleLogin ()


port googleLogout : () -> Cmd msg

logout : Cmd msg
logout = googleLogout ()


port googleRequestContacts : () -> Cmd msg

requestContacts : Cmd msg
requestContacts = googleRequestContacts ()
