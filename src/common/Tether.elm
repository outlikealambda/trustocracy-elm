module Common.Tether exposing
  ( Tether
    (..)
  , data
  )


type Tether a
  = Disjoint a -- has no corresponding database id
  | Attached Int a  -- has a corresponding database id of type a


data : Tether a -> a
data t =
  case t of
    Disjoint a ->
      a
    Attached _ a ->
      a
