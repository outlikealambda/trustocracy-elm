module Model.Extend.Identified exposing
  ( Identified
  , key
  )

type alias Identified a =
  { a
  | id : Int
  }


key : Identified a -> Int
key = .id
