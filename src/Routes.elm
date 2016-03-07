module Routes where

import Effects exposing (Effects)
import RouteParser
import TransitRouter


type Route
  = Home
  | Topics
  | Survey Int
  | Compose Int
  | Browse Int
  | EmptyRoute


routeParsers : List (RouteParser.Matcher Route)
routeParsers =
  [ RouteParser.static Home "/"
  , RouteParser.static Topics "/topics"
  , RouteParser.dyn1 Survey "/topic/" RouteParser.int "/survey"
  , RouteParser.dyn1 Compose "/topic/" RouteParser.int "/compose"
  , RouteParser.dyn1 Browse "/topic/" RouteParser.int "/browse"
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
    EmptyRoute -> "/"


redirect : Route -> Effects ()
redirect route =
  encode route
    |> Signal.send TransitRouter.pushPathAddress
    |> Effects.task
