module ActiveUser
    ( ActiveUser
      ( LoggedIn
      , LoggedOut
      )
    ) where


import User exposing (User)


type ActiveUser
  = LoggedOut
  | LoggedIn User
