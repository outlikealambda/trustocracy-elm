module Auth.Facebook
  ( LoginStatusResponse
  , UserResponse
  , Action(..)
  , Status
  , toString
  , signal
  , address
  ) where


import Signal exposing (Signal, Mailbox)


type Action
  = NoOp
  | Login
  | Logout


type Status
  = Connected
  | NotConnected
  | NotAuthorized


type alias UserResponse =
  { email: String
  , name: String
  , id: String
  }


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


mailbox : Mailbox Action
mailbox =
  Signal.mailbox NoOp


signal : Signal Action
signal =
  mailbox.signal


address : Signal.Address Action
address =
  mailbox.address


toString : Action -> String
toString action =
  case action of
    Login -> "login"
    Logout -> "logout"
    _ -> "noop"
