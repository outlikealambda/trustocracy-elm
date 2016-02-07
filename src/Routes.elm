module Routes where

import Effects exposing (Effects)
import RouteParser
import TransitRouter


type Route
  = Home
  | Connect Int
  | Compose Int


routeParsers : List (RouteParser.Matcher Route)
routeParsers =
  [ RouteParser.static Home "/"
  , RouteParser.dyn1 Connect "/topic/" RouteParser.int "/connect"
  , RouteParser.dyn1 Compose "/topic/" RouteParser.int "/compose"
  ]


decode : String -> Route
decode path =
  RouteParser.match routeParsers path
    |> Maybe.withDefault Home


encode : Route -> String
encode route =
  case route of
    Home -> "/"
    Connect topicId -> "/topic/" ++ toString topicId ++ "/connect"
    Compose topicId -> "/topic/" ++ toString topicId ++ "/compose"


redirect : Route -> Effects ()
redirect route =
  encode route
    |> Signal.send TransitRouter.pushPathAddress
    |> Effects.task
