module Common.Remote exposing
  ( Remote
    (..)
  , withDefault
  )


type Remote a
  = NoRequest
  | Requested
  | Retrieved a


withDefault : a -> Remote a -> a
withDefault default remote =
  case remote of
    Retrieved v ->
      v
    _ ->
      default
