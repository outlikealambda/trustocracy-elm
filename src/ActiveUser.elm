module ActiveUser exposing
    ( ActiveUser
      ( LoggedIn
      , LoggedOut
      )
    )


import User exposing (User)


type ActiveUser
  = LoggedOut
  | LoggedIn User
