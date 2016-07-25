module Common.Backed exposing
  ( Backed
    (..)
  , data
  )


type Backed a
  = Fresh a -- has no corresponding database id
  | Linked Int a  -- has a corresponding database id


data : Backed a -> a
data stored =
  case stored of
    Fresh d ->
      d
    Linked _ d ->
      d
