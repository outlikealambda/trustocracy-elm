module Auth.Facebook exposing
  ( AuthResponse
  , Action
    ( Login
    , Logout
    )
  , toString
  , loginRequests
  , logoutRequests
  , address
  )


type Action
  = NoOp
  | Login
  | Logout


type alias AuthResponse =
  { userID : String
  , accessToken : String
  , expiresIn : Int
  , signedRequest : String
  }

loginRequests :  List String -> Cmd msg
loginRequests =
  let
    filterLoginAction a =
      case a of
        Login -> Just []
        _ -> Nothing
  in
    Signal.filterMap filterLoginAction [] mailbox.signal


logoutRequests : () -> Cmd msg
logoutRequests =
  let
    filterLogoutAction a =
      case a of
        Logout -> Just ()
        _ -> Nothing
  in
    Signal.filterMap filterLogoutAction () mailbox.signal


toString : Action -> String
toString action =
  case action of
    Login -> "login"
    Logout -> "logout"
    _ -> "noop"
