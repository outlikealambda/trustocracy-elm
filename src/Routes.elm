module Routes where

import Effects exposing (Effects)
import Html exposing (Attribute)
import Html.Events exposing (on)
import Json.Decode as Decode
import RouteParser
import String
import TransitRouter


type Route
  = Home
  | Topics
  | Survey Int
  | Compose Int
  | Browse Int
  | Read Int Int
  | EmptyRoute


routeParsers : List (RouteParser.Matcher Route)
routeParsers =
  [ RouteParser.static Home "/"
  , RouteParser.static Topics "/topics"
  , RouteParser.dyn1 Survey "/topic/" RouteParser.int "/survey"
  , RouteParser.dyn1 Compose "/topic/" RouteParser.int "/compose"
  , RouteParser.dyn1 Browse "/topic/" RouteParser.int "/browse"
  , RouteParser.dyn2 Read "/topic/" RouteParser.int "/read/" RouteParser.int ""
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
    Survey topicId -> "/topic/" ++ toString topicId ++ "/survey"
    Compose topicId -> "/topic/" ++ toString topicId ++ "/compose"
    Browse topicId -> "/topic/" ++ toString topicId ++ "/browse"
    Read topicId opinionId ->
      String.concat
        [ "/topic/"
        , toString topicId
        , "/read/"
        , toString opinionId
        ]
    EmptyRoute -> "/"


redirect : Route -> Effects ()
redirect route =
  encode route
    |> Signal.send TransitRouter.pushPathAddress
    |> Effects.task


goToRoute : Route -> Attribute
goToRoute route =
  on
    "click"
    Decode.value
    (\_ -> Signal.message TransitRouter.pushPathAddress <| encode route)
