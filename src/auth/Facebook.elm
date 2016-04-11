module Auth.Facebook
  ( LoginStatusResponse
  , AuthResponse
  , Action
    ( Login
    , Logout
    )
  , Status
  , toString
  , loginRequests
  , logoutRequests
  , address
  ) where


import Signal exposing (Signal, Mailbox)


type Action
  = NoOp
  | Login
  | Logout


type Status
  = Connected
  | NotAuthorized
  | Unknown


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


loginRequests : Signal (List String)
loginRequests =
  let
    filterLoginAction a =
      case a of
        Login -> Just []
        _ -> Nothing
  in
    Signal.filterMap filterLoginAction [] mailbox.signal


logoutRequests : Signal ()
logoutRequests =
  let
    filterLogoutAction a =
      case a of
        Logout -> Just ()
        _ -> Nothing
  in
    Signal.filterMap filterLogoutAction () mailbox.signal


address : Signal.Address Action
address =
  mailbox.address


toString : Action -> String
toString action =
  case action of
    Login -> "login"
    Logout -> "logout"
    _ -> "noop"
