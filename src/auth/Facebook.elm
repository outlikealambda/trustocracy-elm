module Auth.Facebook
  ( LoginStatusResponse
  , AuthResponse
  , Status
  , toString
  ) where


import Signal exposing (Signal, Mailbox)


type Status
  = Connected
  | NotConnected
  | NotAuthorized


type alias LoginStatusResponse =
  { status : String
  , authResponse : Maybe AuthResponse
  }


type alias AuthResponse =
  { userID : String
  , accessToken : String
  , expiresIn : Int
  , signedRequest : String
  }


mailbox : Mailbox LoginStatusResponse
mailbox =
  Signal.mailbox { status = "", authResponse = Nothing }


toString : LoginStatusResponse -> String
toString response =
  response.status
