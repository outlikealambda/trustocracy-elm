module Model.Extend.Writeable exposing
  ( Writeable
  , Status
    (..)
  )


type alias Writeable a =
  { a
  | writeStatus : Status
  }


type Status
  = Written
  | Writing
  | Failed
  | None
