port module Auth.Facebook exposing
  ( AuthResponse
  , login
  , logout
  , authResponses
  )


type alias AuthResponse =
  { userID : String
  , accessToken : String
  , expiresIn : Int
  , signedRequest : String
  }


port fbAuthResponses : (Maybe AuthResponse -> msg) -> Sub msg
authResponses : (Maybe AuthResponse -> msg) -> Sub msg
authResponses = fbAuthResponses


port fbLogin : () -> Cmd msg
login : Cmd msg
login = fbLogin ()


port fbLogout : () -> Cmd msg
logout : Cmd msg
logout = fbLogout ()
