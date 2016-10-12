module Common.Remote exposing
  ( Remote
    (..)
  , requested
  , retrieved
  )


type Remote a
  = Requested a
  | Retrieved a


requested : a -> Remote a
requested = Requested


retrieved : a -> Remote a
retrieved = Retrieved
