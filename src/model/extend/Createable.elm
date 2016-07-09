module Model.Extend.Createable exposing
  ( Createable
  )


type alias Createable a =
  { a
  | id : Maybe Int
  }
