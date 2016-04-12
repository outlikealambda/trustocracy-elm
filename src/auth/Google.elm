module Auth.Google
  ( AuthResponse
  , loginRequests
  , logoutRequests
  , Action
    ( Login
    , Logout
    )
  , address
  ) where


type alias AuthResponse =
  { idToken: String
  , expiresAt: Int
  }


type Action
  = NoOp
  | Login
  | Logout


mailbox : Signal.Mailbox Action
mailbox =
  Signal.mailbox NoOp


address : Signal.Address Action
address =
  mailbox.address


signal : Signal Action
signal =
  mailbox.signal


loginRequests : Signal ()
loginRequests =
  let
    filterLoginAction a =
      case a of
        Login -> Just ()
        _ -> Nothing
  in
    Signal.filterMap filterLoginAction () mailbox.signal


logoutRequests : Signal ()
logoutRequests =
  let
    filterLogoutAction a =
      case a of
        Logout -> Just ()
        _ -> Nothing
  in
    Signal.filterMap filterLogoutAction () mailbox.signal
