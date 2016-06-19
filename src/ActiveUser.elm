module ActiveUser exposing
    ( ActiveUser
      ( LoggedIn
      , LoggedOut
      )
    )


import Model.User exposing (User)


type ActiveUser
  = LoggedOut
  | LoggedIn User
