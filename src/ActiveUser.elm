module ActiveUser
    ( ActiveUser(LoggedIn, LoggedOut)
    , save
    , clear
    , updates
    , toMaybe
    , fromMaybe
    ) where


import Signal exposing (Address, Mailbox)


import User exposing (User)


type ActiveUser
  = LoggedOut
  | LoggedIn User


type Action
  = NoOp
  | SaveActiveUser User
  | ClearActiveUser


mailbox : Mailbox Action
mailbox =
  Signal.mailbox NoOp


clear : Address ()
clear =
  Signal.forwardTo mailbox.address (\_ -> ClearActiveUser)


save : Address User
save =
  Signal.forwardTo mailbox.address SaveActiveUser


updates : Signal ActiveUser
updates =
  let
    extractActiveUser : Action -> Maybe ActiveUser
    extractActiveUser action =
      case (Debug.log "activeUser signal" action) of
        SaveActiveUser user ->
          Just (LoggedIn user)

        ClearActiveUser ->
          Just LoggedOut

        _ ->
          Nothing
  in
    Signal.filterMap extractActiveUser LoggedOut mailbox.signal


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
