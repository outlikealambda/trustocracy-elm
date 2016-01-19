module NearestOpinions (Model, Action(SetUser), init, view, update) where

import Effects exposing (Effects)
import Task
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Json.Decode as Json exposing ((:=))

import OpinionPathGroup as OPG
import OpinionPath as OP
import Dict
import Http


type alias Key = String
type alias Uid = Int
type alias Tid = Int

type alias Ops = List OP.Model


type alias Model =
  { uid: Uid,
    tid: Tid,
    raw : Ops
  , buckets : Dict.Dict Key OPG.Model -- opinion paths bucketed by key
  }


type Action
  = SetRaw (Maybe Ops)
  | SetUser Uid
  | SetTopic Tid
  | SubMsg Key OPG.Action


init : Tid -> Uid -> (Model, Effects Action)
init tid uid =
  ( Model tid uid [] Dict.empty
  , getNearestOpinions tid uid
  )


update : Action -> Model -> (Model, Effects Action)
update message model =
  case message of
    SetRaw maybeOps ->
      case maybeOps of

        Nothing -> (model, Effects.none)

        Just ops ->
          let newBuckets =
            bucketList OPG.opinerKeyGen ops Dict.empty |> Dict.map toOPG

          in
            ( { model
              | raw = ops
              , buckets = newBuckets }
            , Effects.none )

    SetTopic tid ->
      ( { model | tid = tid },
      getNearestOpinions tid model.uid)

    SetUser uid ->
      ( { model | uid = uid }
      , getNearestOpinions model.tid uid)

    SubMsg key subMsg ->
      let
          maybeBucket =
            Dict.get key model.buckets

          (newBuckets, fx) =
            case maybeBucket of
              Nothing ->
                ( model.buckets
                , Effects.none)
              Just bucket ->
                let
                    (bucketUpdate, fx) =
                      OPG.update subMsg bucket
                in
                    ( Dict.insert key bucket model.buckets
                    , fx )
      in
          ( { model | buckets = newBuckets }
          , Effects.map (SubMsg key) fx
          )

  -- let dict =
  --   bucketList OPG.opinerKeyGen opg.paths Dict.empty
  --   |> Dict.map mapToOPG
  -- in Model opg dict

getNearestOpinions : Tid -> Uid -> Effects Action
getNearestOpinions tid uid =
  buildUrl tid uid
  |> Http.get opsDecoder
  |> Task.toMaybe
  |> Task.map SetRaw
  |> Effects.task

opsDecoder : Json.Decoder Ops
opsDecoder =
  "paths" := Json.list OP.decoder

buildUrl : Tid -> Uid -> String
buildUrl tid uid =
  "http://localhost:3714/api/user/" ++ toString uid ++ "/topic/" ++ toString tid ++ "/opinions"

view : Signal.Address Action -> Model -> List Html
view address nops =
  Dict.toList nops.buckets
  |> List.map (viewOPG address)


viewOPG : Signal.Address Action -> (Key, OPG.Model) -> Html
viewOPG address (key, opg) =
  OPG.view (Signal.forwardTo address (SubMsg key)) opg


toOPG : comparable -> List OP.Model -> OPG.Model
toOPG key ops = OPG.Model False ops


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
