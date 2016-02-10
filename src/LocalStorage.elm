module LocalStorage
    ( saveActiveUserAddress
    , clearActiveUserAddress
    , saveActiveUserSignal
    ) where


import Signal exposing (Address, Mailbox)


import User exposing (User)


type Action
  = NoOp
  | SaveActiveUser User
  | ClearActiveUser


mailbox : Mailbox Action
mailbox =
  Signal.mailbox NoOp


clearActiveUserAddress : Address ()
clearActiveUserAddress = Signal.forwardTo mailbox.address (\_ -> ClearActiveUser)


saveActiveUserAddress : Address User
saveActiveUserAddress = Signal.forwardTo mailbox.address SaveActiveUser


saveActiveUserSignal : Signal (Maybe User)
saveActiveUserSignal =
  let
    extractActiveUser : Action -> Maybe (Maybe User)
    extractActiveUser action =
      case action of
        SaveActiveUser user -> Just (Just user)
        ClearActiveUser -> Just Nothing
        _ -> Nothing
  in
    Signal.filterMap extractActiveUser (Just User.empty) mailbox.signal
