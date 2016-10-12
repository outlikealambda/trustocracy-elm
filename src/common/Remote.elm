module Common.Remote exposing
  ( Remote
    (..)
  )


type Remote a
  = NoRequest
  | Requested
  | Retrieved a
