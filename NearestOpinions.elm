module NearestOpinions (Model, init, view) where

import Html exposing (Html, div)
import Html.Attributes exposing (class)

import OpinionPathGroup as OPG
import OpinionPath as OP
import Dict

type alias Key = String

type alias Model =
  { opg : OPG.Model -- all opinion paths
  , buckets : Dict.Dict Key OPG.Model -- opinion paths bucketed by key
  }

init : OPG.Model -> Model
init opg =
  let dict =
    bucketList OPG.opinerKeyGen opg.paths Dict.empty
    |> Dict.map mapToOPG
  in Model opg dict

view : Model -> List Html
view nops =
  List.map OPG.view (Dict.values nops.buckets)

mapToOPG : comparable -> List OP.Model -> OPG.Model
mapToOPG key ops = OPG.Model False ops

-- type Group
--   = Friend
--   | Opiner

bucketList : (a -> comparable) -> List a -> Dict.Dict comparable (List a) -> Dict.Dict comparable (List a)
bucketList keyGen opinions dict =
  case opinions of
    o::os ->
      bucketListItem (keyGen o) o dict
      |> bucketList keyGen os
    [] -> dict

bucketListItem : comparable -> a -> Dict.Dict comparable (List a) -> Dict.Dict comparable (List a)
bucketListItem key v dict =
  Dict.insert key (v :: safeGetList key dict) dict

safeGetList : comparable -> Dict.Dict comparable (List a) -> List a
safeGetList key dict = Maybe.withDefault [] (Dict.get key dict)
