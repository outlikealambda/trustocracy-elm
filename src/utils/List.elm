module Utils.List exposing
  ( groupBy
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
      List.partition (\keyedItem -> fst pair == fst keyedItem) keyedItems
    appendTo =
      case matched of
        [] -> []
        first::rest -> snd first
  in
    ( fst pair, snd pair :: appendTo ) :: unmatched
