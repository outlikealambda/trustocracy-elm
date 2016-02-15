module Opinion.Model
  ( Model
  , empty
  , decoder
  , setExpanded
  , setText
  , fetch
  , fetchByUserTopic
  ) where


import String
import Json.Decode as Json exposing ((:=))
import Http
import Task
import Effects exposing (Effects)


import Opinion.Credentials as Credentials


type alias Model =
  { oid: Int
  , expanded: Bool
  , text : String
  , snippet : String
  , credentials : Credentials.Model
  }


fromApi : Int -> String -> Model
fromApi oid text =
  { oid = oid
  , expanded = False
  , text = text
  , snippet = snippetize 200 text
  , credentials = Credentials.init
  }

empty : Model
empty =
  Model -1 False "" "" Credentials.init

setExpanded : Model -> Model
setExpanded m =
  { m | expanded = True }


decoder : Json.Decoder Model
decoder =
  Json.object2 fromApi
    ( "id" := Json.int )
    ( "text" := Json.string )


setText : String -> Model -> Model
setText text model =
  { model
  | text = text
  , snippet = snippetize 200 text }


snippetize : Int -> String -> String
snippetize maxLength s =
  -- add a space on the end to avoid removing the last word every time
  s ++ " "
    |> String.indexes " "
    |> maxValLessThan maxLength
    |> maybeSlice s


maybeSlice : String -> Maybe Int -> String
maybeSlice s maybeBound =
  case maybeBound of
    Nothing -> s
    Just bound ->
      if bound < String.length s then
        (String.slice 0 bound s) ++ "..."
      else
        s


maxValLessThan : Int -> List Int -> Maybe Int
maxValLessThan maxVal ns =
  List.filter ((>) maxVal) ns
    |> List.maximum


fetch : Int -> Effects Model
fetch opinionId =
  buildFetchUrl opinionId
    |> Http.get decoder
    |> Task.toMaybe
    |> Task.map (Maybe.withDefault empty)
    |> Effects.task


buildFetchUrl : Int -> String
buildFetchUrl opinionId =
  String.concat
    [ "http://localhost:3714/api/opinion/"
    , toString opinionId
    ]


fetchByUserTopic : Int -> Int -> Effects Model
fetchByUserTopic userId topicId =
  buildFetchByUserTopicUrl userId topicId
    |> Http.get decoder
    |> Task.toMaybe
    |> Task.map (Maybe.withDefault empty)
    |> Effects.task


buildFetchByUserTopicUrl : Int -> Int -> String
buildFetchByUserTopicUrl userId topicId =
  String.concat
    [ "http://localhost:3714/api/user/"
    , toString userId
    , "/topic/"
    , toString topicId
    , "/opinion"
    ]
