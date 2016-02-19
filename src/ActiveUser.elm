module ActiveUser
    ( ActiveUser
      ( LoggedIn
      , LoggedOut
      )
    , save
    , clear
    , signal
    , toMaybe
    , fromMaybe
    , toUser
    ) where


import Signal exposing (Address, Mailbox)


import User exposing (User)


type ActiveUser
  = LoggedOut
  | LoggedIn User


mailbox : Mailbox ActiveUser
mailbox =
  Signal.mailbox LoggedOut


clear : Address ()
clear =
  Signal.forwardTo mailbox.address (\_ -> LoggedOut)


save : Address User
save =
  Signal.forwardTo mailbox.address LoggedIn


signal : Signal ActiveUser
signal =
  mailbox.signal


toMaybe : ActiveUser -> Maybe User
toMaybe activeUser =
  case activeUser of
    LoggedOut ->
      Nothing

    LoggedIn user ->
      Just user


fromMaybe : Maybe User -> ActiveUser
fromMaybe maybeUser =
  case maybeUser of
    Nothing ->
      LoggedOut

    Just user ->
      LoggedIn user


toUser : ActiveUser -> User
toUser activeUser =
  case activeUser of
    LoggedIn user ->
      user

    LoggedOut ->
      User.empty
