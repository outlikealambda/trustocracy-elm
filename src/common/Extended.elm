module Common.Extended exposing
  ( Extended
    ( Basic
    , Complex
    )
  )

type Extended a b
  = Basic a
  | Complex a b
