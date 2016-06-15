module Utils.Dict exposing
  ( createListBuckets
  , updateWithCmd
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


updateWithCmd : Int -> (a -> (a, Cmd b)) -> Dict Int a -> (Dict Int a, Cmd b)
updateWithCmd key updater dict =
  let
    maybeGet =
      Dict.get key dict
  in
    case maybeGet of
      Nothing ->
        dict ! []
      Just a ->
        let
          (updateValue, updateFx) =
            updater a
        in
          Dict.insert key updateValue dict ! [ updateFx ]
