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


loginRequests : () -> Cmd msg
loginRequests =
  let
    filterLoginAction a =
      case a of
        Login -> Just ()
        _ -> Nothing
  in
    Signal.filterMap filterLoginAction () mailbox.signal


logoutRequests : () -> Cmd msg
logoutRequests =
  let
    filterLogoutAction a =
      case a of
        Logout -> Just ()
        _ -> Nothing
  in
    Signal.filterMap filterLogoutAction () mailbox.signal


contactsRequests : () -> Cmd msg
contactsRequests =
  let
    filterContactsAction a =
      case a of
        Contacts -> Just ()
        _ -> Nothing
  in
    Signal.filterMap filterContactsAction () mailbox.signal
