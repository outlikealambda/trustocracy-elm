port module Auth.Cookie exposing (..)

port trustoLogout : () -> Cmd msg


logout : Cmd msg
logout =
  trustoLogout ()
