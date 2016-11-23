module Utils.List exposing
  ( groupBy
  , singleton
  , replace
  )


-- not the standard function groupBy, this groups _all_ elements
-- in a list which satisfy an equality check (not just
-- consecutive elements)
groupBy : (a -> comparable) -> List a -> List (comparable, List a)
groupBy keyGen items =
  let
    pairs =
      List.map
        (\item -> (keyGen item, item))
        items

  in
    combinePairs pairs []


-- private
combinePairs : List (comparable, a) -> List (comparable, List a) -> List (comparable, List a)
combinePairs pairs combined =
  case pairs of
    [] ->
      combined
    pair::remainder ->
      combinePairs
        remainder
        (insertOrAppend pair combined)


insertOrAppend : (comparable, a) -> List (comparable, List a) -> List (comparable, List a)
insertOrAppend pair keyedItems =
  let
    (matched, unmatched) =
      List.partition (\keyedItem -> Tuple.first pair == Tuple.first keyedItem) keyedItems
    appendTo =
      case matched of
        [] -> []
        first::rest -> Tuple.second first
  in
    ( Tuple.first pair, Tuple.second pair :: appendTo ) :: unmatched


singleton : a -> List a
singleton = flip (::) []


replace : (a -> Bool) -> a -> List a -> List a
replace isMatch newItem items =
  List.filter (not << isMatch) items
    |> List.append [ newItem ]
