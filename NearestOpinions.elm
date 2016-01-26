module NearestOpinions (Model, Action(SetUser), init, view, update) where

import Effects exposing (Effects)
import Task
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Json.Decode as Json exposing ((:=))
import Dict
import String
import Http

import OpinionPathGroup as OPG
import OpinionPath as OP
import User exposing (User)
import Topic exposing (Topic)


type alias Key = Int

type alias Ops = List OP.Model


type alias Model =
  { user : User
  , topic : Topic
  , raw : Ops
  , buckets : Dict.Dict Key OPG.Model -- opinion paths bucketed by key
  }


type Action
  = SetRaw Ops
  | SetUser User
  | SetTopic Topic
  | SetOpinions (List (Key, String))
  | SubMsg Key OPG.Action


init : User -> Topic -> (Model, Effects Action)
init user topic =
  ( Model user topic [] Dict.empty
  , Effects.none
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
              |> Dict.map (toOPG model.topic)

          in
            ( { model
              | raw = ops
              , buckets = newBuckets }
            , getOpinions (Dict.keys newBuckets) )

    SetOpinions rawOpinions ->
      ( model
      , Effects.batch (List.map createSetOpinionEffect rawOpinions)
      )

    SetTopic topic ->
      ( { model | topic = topic }
      , getNearestOpinions topic model.user)

    SetUser user ->
      ( { model | user = user }
      , getNearestOpinions model.topic user)

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

getNearestOpinions : Topic -> User -> Effects Action
getNearestOpinions topic user =
  buildNearestOpinionsUrl topic user.id
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


buildNearestOpinionsUrl : Int -> Int -> String
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


toOPG : Topic -> Key -> List OP.Model -> OPG.Model
toOPG topic key ops = OPG.fromOpinionPaths key topic ops


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
