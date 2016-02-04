module NearestOpinions
  ( Model
  , Action(SetUser)
  , init
  , view
  , update
  ) where

import Effects exposing (Effects)
import Task
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Json.Decode as Json exposing ((:=))
import Dict
import String
import Http

import OpinionPathGroup as OGroup
import OpinionPath as OPath
import User exposing (User)
import Topic exposing (Topic)


type alias Key = Int
type alias OPaths = List OPath.Model


type alias Model =
  { user : User
  , topic : Topic
  , rawPaths : OPaths
  , buckets : Dict.Dict Key OGroup.Model -- opinion paths bucketed by key
  }


type Action
  = SetRaw OPaths
  | SetUser User
  | SetTopic Topic
  | OpgMsg Key OGroup.Action


init : User -> Topic -> (Model, Effects Action)
init user topic =
  ( Model user topic [] Dict.empty
  , getNearestOpinions topic user
  )


update : Action -> Model -> (Model, Effects Action)
update message model =
  case message of
    SetRaw opaths ->
      case opaths of

        [] -> (model, Effects.none)

        opaths ->
          let
            (groups, fxs) =
              OGroup.initGroups opaths
                |> List.map (\(g, fx) -> (g, Effects.map (OpgMsg g.groupId) fx))
                |> List.unzip
            buckets =
              OGroup.toDict groups
          in
            ( { model
              | rawPaths = opaths
              , buckets = buckets
              }
            , Effects.batch fxs
            )

    SetTopic topic ->
      ( { model | topic = topic }
      , getNearestOpinions topic model.user)

    SetUser user ->
      ( { model | user = user }
      , getNearestOpinions model.topic user)

    OpgMsg key subMsg ->
      case Dict.get key model.buckets of

        Nothing ->
          ( model
          , Effects.none )

        Just bucket ->
          let
            (updatedBucket, fx) =
              OGroup.update subMsg bucket
            updatedBuckets =
              Dict.insert key updatedBucket model.buckets
          in
            ( { model | buckets = updatedBuckets }
            , Effects.map (OpgMsg key) fx
            )


getNearestOpinions : Topic -> User -> Effects Action
getNearestOpinions topic user =
  buildNearestOpinionsUrl topic user.id
    |> Http.get opathsDecoder
    |> Task.toMaybe
    |> Task.map (Maybe.withDefault [])
    |> Task.map SetRaw
    |> Effects.task


opathsDecoder : Json.Decoder OPaths
opathsDecoder =
  "paths" := Json.list OPath.decoder


buildNearestOpinionsUrl : Int -> Int -> String
buildNearestOpinionsUrl tid uid =
  String.concat
    [ "http://localhost:3714/api/user/"
    , toString uid
    , "/topic/"
    , toString tid
    , "/opinions"
    ]


view : Signal.Address Action -> Model -> List Html
view address nops =
  Dict.toList nops.buckets
    |> List.map (viewOGroup address)


viewOGroup : Signal.Address Action -> (Key, OGroup.Model) -> Html
viewOGroup address (key, opg) =
  OGroup.view (Signal.forwardTo address (OpgMsg key)) opg
