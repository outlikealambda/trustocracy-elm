module LocalStorage
    ( ActiveUser(LoggedIn, LoggedOut)
    , saveActiveUserAddress
    , clearActiveUserAddress
    , saveActiveUserSignal
    , toMaybe
    , fromMaybe
    ) where


import Signal exposing (Address, Mailbox)


import User exposing (User)


type Action
  = NoOp
  | SaveActiveUser User
  | ClearActiveUser


type ActiveUser
  = LoggedOut
  | LoggedIn User


mailbox : Mailbox Action
mailbox =
  Signal.mailbox NoOp


clearActiveUserAddress : Address ()
clearActiveUserAddress =
  Signal.forwardTo mailbox.address (\_ -> ClearActiveUser)


saveActiveUserAddress : Address User
saveActiveUserAddress =
  Signal.forwardTo mailbox.address SaveActiveUser


saveActiveUserSignal : Signal ActiveUser
saveActiveUserSignal =
  let
    extractActiveUser : Action -> Maybe ActiveUser
    extractActiveUser action =
      case action of
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
