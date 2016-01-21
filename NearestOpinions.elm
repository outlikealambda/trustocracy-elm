module NearestOpinions (Model, Action(SetUser), init, view, update) where

import Effects exposing (Effects)
import Task
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Json.Decode as Json exposing ((:=))

import OpinionPathGroup as OPG
import OpinionPath as OP
import Dict
import String
import Http


type alias Key = Int
type alias Uid = Int
type alias Tid = Int

type alias Ops = List OP.Model


type alias Model =
  { uid: Uid
  , tid: Tid
  , raw : Ops
  , buckets : Dict.Dict Key OPG.Model -- opinion paths bucketed by key
  }


type Action
  = SetRaw Ops
  | SetUser Uid
  | SetOpinions (List (Key, String))
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
    SetRaw ops ->
      case ops of

        [] -> (model, Effects.none)

        ops ->
          let newBuckets =
            bucketList OPG.opinionKeyGen ops Dict.empty
              |> Dict.map toOPG

          in
            ( { model
              | raw = ops
              , buckets = newBuckets }
            , getOpinions (Dict.keys newBuckets) )

    SetOpinions rawOpinions ->
      ( model
      , Effects.batch (Debug.log "messages" (List.map createSetOpinionEffect (Debug.log "raw-opinions" rawOpinions)))
      )


    SetTopic tid ->
      ( { model | tid = tid }
      , getNearestOpinions tid model.uid)

    SetUser uid ->
      ( { model | uid = uid }
      , getNearestOpinions model.tid uid)

    SubMsg key subMsg ->
      let (newBuckets, fx) =
            case Dict.get key model.buckets of
              Nothing ->
                ( model.buckets
                , Effects.none )
              Just bucket ->
                let
                    (updatedBucket, fx) =
                      OPG.update subMsg bucket
                    updatedBuckets =
                      Dict.insert key updatedBucket model.buckets
                in
                    ( updatedBuckets
                    , fx )
      in
          ( { model | buckets = newBuckets }
          , Effects.map (SubMsg key) fx
          )

createSetOpinionEffect : (Key, String) -> Effects Action
createSetOpinionEffect rawOpinion =
  SubMsg (fst rawOpinion) (OPG.SetOpinion rawOpinion)
    |> Task.succeed
    |> Effects.task

getNearestOpinions : Tid -> Uid -> Effects Action
getNearestOpinions tid uid =
  buildNearestOpinionsUrl tid uid
    |> Http.get opsDecoder
    |> Task.toMaybe
    |> Task.map (Maybe.withDefault [])
    |> Task.map SetRaw
    |> Effects.task


opsDecoder : Json.Decoder Ops
opsDecoder =
  "paths" := Json.list OP.decoder


getOpinions : List Key -> Effects Action
getOpinions ids =
  buildOpinionsUrl ids
    |> Http.get opinionsRawDecoder
    |> Task.toMaybe
    |> Task.map (Maybe.withDefault [])
    |> Task.map SetOpinions
    |> Effects.task


opinionsRawDecoder : Json.Decoder (List (Key, String))
opinionsRawDecoder =
  let opinionDec =
        Json.object2 (,)
          ("id" := Json.int)
          ("text" := Json.string)
  in
      Json.list opinionDec


buildNearestOpinionsUrl : Tid -> Uid -> String
buildNearestOpinionsUrl tid uid =
  String.concat
    [ "http://localhost:3714/api/user/"
    , toString uid
    , "/topic/"
    , toString tid
    , "/opinions"
    ]


buildOpinionsUrl : List Int -> String
buildOpinionsUrl ids =
  List.map toString ids
    |> String.join ","
    |> (++) "http://localhost:3714/api/opinions/"


view : Signal.Address Action -> Model -> List Html
view address nops =
  Dict.toList nops.buckets
    |> List.map (viewOPG address)


viewOPG : Signal.Address Action -> (Key, OPG.Model) -> Html
viewOPG address (key, opg) =
  OPG.view (Signal.forwardTo address (SubMsg key)) opg


toOPG : Key -> List OP.Model -> OPG.Model
toOPG key ops = OPG.fromOpinionPaths key ops


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
