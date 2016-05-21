module Auth.Google exposing
  ( AuthResponse
  , loginRequests
  , logoutRequests
  , contactsRequests
  , Action
    ( Login
    , Logout
    , Contacts
    )
  , address
  )


type alias AuthResponse =
  { idToken: String
  , expiresAt: Int
  , accessToken: String
  , scope: String
  }


type Action
  = NoOp
  | Login
  | Logout
  | Contacts


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


contactsRequests : Signal ()
contactsRequests =
  let
    filterContactsAction a =
      case a of
        Contacts -> Just ()
        _ -> Nothing
  in
    Signal.filterMap filterContactsAction () mailbox.signal
