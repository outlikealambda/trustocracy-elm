port module Auth.Cookie exposing (..)

port trustoLogout : () -> Cmd msg
logout : Cmd ()
logout =
  trustoLogout ()
