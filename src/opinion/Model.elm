module Opinion.Model
  ( Model
  , empty
  , decoder
  , setExpanded
  , setText
  , getInfluence
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
  , text : String
  , influence : Int
  , credentials : Credentials.Model
  , expanded: Bool
  , snippet : String
  , fetched : Bool
  }


fromApi : Int -> Int -> String -> Model
fromApi oid influence text =
  { oid = oid
  , influence = influence
  , text = text
  , credentials = Credentials.init
  , snippet = snippetize 200 text
  , expanded = False
  , fetched = True
  }


empty : Model
empty =
  { oid = -1
  , influence = 0
  , text = ""
  , credentials = Credentials.init
  , expanded = False
  , snippet = ""
  , fetched = False
  }


setExpanded : Model -> Model
setExpanded m =
  { m | expanded = True }


decoder : Json.Decoder Model
decoder =
  Json.object3 fromApi
    ( "id" := Json.int )
    ( "influence" := Json.int )
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


getInfluence : Model -> Int
getInfluence = .influence


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
