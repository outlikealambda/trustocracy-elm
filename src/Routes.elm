module Routes exposing (..)

import Html exposing (Attribute)
import Html.Attributes exposing (href)
import Html.Events exposing (on, onWithOptions)
import Json.Decode as Decode
import RouteParser
import String


type Route
  = Home
  | Topics
  -- | Survey TopicId
  | Compose TopicId
  | Read TopicId OpinionId
  | Explore TopicId
  | UserDelegates
  | EmptyRoute


type alias TopicId = Int
type alias OpinionId = Int


routeParsers : List (RouteParser.Matcher Route)
routeParsers =
  [ RouteParser.static Home "/"
  , RouteParser.static Topics "/topics"
  -- , RouteParser.dyn1 Survey "/topic/" RouteParser.int "/survey"
  , RouteParser.dyn1 Compose "/topic/" RouteParser.int "/compose"
  , RouteParser.dyn1 Explore "/topic/" RouteParser.int "/explore"
  , RouteParser.dyn2 Read "/topic/" RouteParser.int "/read/" RouteParser.int ""
  , RouteParser.static UserDelegates "/my/trust"
  ]


decode : String -> Route
decode path =
  RouteParser.match routeParsers path
    |> Maybe.withDefault Home


encode : Route -> String
encode route =
  case route of
    Home -> "/"
    Topics -> "/topics"
    -- Survey topicId -> "/topic/" ++ toString topicId ++ "/survey"
    Compose topicId -> "/topic/" ++ toString topicId ++ "/compose"
    Read topicId opinionId ->
      String.concat
        [ "/topic/"
        , toString topicId
        , "/read/"
        , toString opinionId
        ]
    Explore topicId -> "/topic/" ++ toString topicId ++ "/explore"
    UserDelegates -> "/my/trust"
    EmptyRoute -> "/"


goToRoute : m -> Attribute m
goToRoute msg =
  on
    "click"
    (Decode.map (\_ -> msg) Decode.value)


clickTo : (Route -> m) -> Route -> List (Attribute m)
clickTo onRoute route =
  let
    path =
      encode route
    msg =
      onRoute route
  in
    [ href path
    , onWithOptions
      "click"
      { stopPropagation = True, preventDefault = True }
      (Decode.map (\_ -> msg) Decode.value)
    ]
