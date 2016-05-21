module Utils.Dict exposing
  ( createListBuckets
  )


import Dict exposing (Dict)


-- previously used by Plot.initPlots, but replaced with Utils.List.groupBy
-- could still be useful for creating a dict from a List
createListBuckets : (a -> comparable) -> List a -> Dict comparable (List a)
createListBuckets f l =
  bucketList f l Dict.empty


bucketList : (a -> comparable) -> List a -> Dict comparable (List a) -> Dict comparable (List a)
bucketList keyGen opinions dict =
  case opinions of
    o::os ->
      bucketListItem (keyGen o) o dict
      |> bucketList keyGen os
    [] -> dict


bucketListItem : comparable -> a -> Dict comparable (List a) -> Dict comparable (List a)
bucketListItem key v dict =
  Dict.insert key (v :: safeGetList key dict) dict


safeGetList : comparable -> Dict comparable (List a) -> List a
safeGetList key dict = Maybe.withDefault [] (Dict.get key dict)
