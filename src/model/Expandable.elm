module Model.Expandable exposing
  ( Expandable
  , Status (..)
  , expand
  , collapse
  )

type alias Expandable a =
  { a
  | status : Status }

type Status = Expanded | Collapsed


expand : Expandable a -> Expandable a
expand expandable =
  { expandable | status = Expanded }


collapse : Expandable a -> Expandable a
collapse expandable =
  { expandable | status = Collapsed }
