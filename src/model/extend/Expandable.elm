module Model.Extend.Expandable exposing
  ( Expandable
  , Inflation (..)
  , expand
  , collapse
  )

type alias Expandable a =
  { a
  | inflation : Inflation }

type Inflation = Expanded | Collapsed


expand : Expandable a -> Expandable a
expand expandable =
  { expandable | inflation = Expanded }


collapse : Expandable a -> Expandable a
collapse expandable =
  { expandable | inflation = Collapsed }
