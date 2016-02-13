module Opinion.Connector
  ( Model
  , Action(SetUser)
  , empty
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

import Opinion.Group as Group
import Opinion.Path as Path
import User exposing (User)
import Topic.Model exposing (Topic)


type alias Key = Int
type alias Paths = List Path.Model


type alias Model =
  { user : User
  , topic : Topic
  , rawPaths : Paths
  , buckets : Dict.Dict Key Group.Model -- opinion paths bucketed by key
  }


type Action
  = SetRaw Paths
  | SetUser User
  | SetTopic Topic
  | OpgMsg Key Group.Action


empty : Model
empty = Model User.empty Topic.Model.empty [] Dict.empty


init : User -> Topic -> (Model, Effects Action)
init user topic =
  ( Model user topic [] Dict.empty
  , getConnectedOpinions topic user
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
              Group.initGroups opaths
                |> List.map (\(g, fx) -> (g, Effects.map (OpgMsg g.groupId) fx))
                |> List.unzip
            buckets =
              Group.toDict groups
          in
            ( { model
              | rawPaths = opaths
              , buckets = buckets
              }
            , Effects.batch fxs
            )

    SetTopic topic ->
      ( { model | topic = topic }
      , getConnectedOpinions topic model.user)

    SetUser user ->
      ( { model | user = user }
      , getConnectedOpinions model.topic user)

    OpgMsg key subMsg ->
      case Dict.get key model.buckets of

        Nothing ->
          ( model
          , Effects.none )

        Just bucket ->
          let
            (updatedBucket, fx) =
              Group.update subMsg bucket
            updatedBuckets =
              Dict.insert key updatedBucket model.buckets
          in
            ( { model | buckets = updatedBuckets }
            , Effects.map (OpgMsg key) fx
            )


getConnectedOpinions : Topic -> User -> Effects Action
getConnectedOpinions topic user =
  buildConnectedOpinionsUrl topic.id user.id
    |> Http.get opathsDecoder
    |> Task.toMaybe
    |> Task.map (Maybe.withDefault [])
    |> Task.map SetRaw
    |> Effects.task


opathsDecoder : Json.Decoder Paths
opathsDecoder =
  "paths" := Json.list Path.decoder


buildConnectedOpinionsUrl : Int -> Int -> String
buildConnectedOpinionsUrl tid uid =
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
    |> List.map (viewGroup address)


viewGroup : Signal.Address Action -> (Key, Group.Model) -> Html
viewGroup address (key, opg) =
  Group.view (Signal.forwardTo address (OpgMsg key)) opg
