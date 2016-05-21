module Routes exposing (..)

import Platform.Cmd exposing (Cmd)
import Html exposing (Attribute)
import Html.Attributes exposing (href)
import Html.Events exposing (on, onWithOptions)
import Json.Decode as Decode
import RouteParser
import String
import Transit


type Route
  = Home
  | Topics
  | Survey Int
  | Compose Int
  | Read Int Int
  | UserDelegates
  | EmptyRoute


routeParsers : List (RouteParser.Matcher Route)
routeParsers =
  [ RouteParser.static Home "/"
  , RouteParser.static Topics "/topics"
  , RouteParser.dyn1 Survey "/topic/" RouteParser.int "/survey"
  , RouteParser.dyn1 Compose "/topic/" RouteParser.int "/compose"
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
    Survey topicId -> "/topic/" ++ toString topicId ++ "/survey"
    Compose topicId -> "/topic/" ++ toString topicId ++ "/compose"
    Read topicId opinionId ->
      String.concat
        [ "/topic/"
        , toString topicId
        , "/read/"
        , toString opinionId
        ]
    UserDelegates -> "/my/trust"
    EmptyRoute -> "/"


redirect : Route -> Cmd ()
redirect route =
  encode route
    |> Signal.send TransitRouter.pushPathAddress
    |> Cmd.task


goToRoute : m -> Attribute m
goToRoute msg =
  on
    "click"
    (Decode.map (\_ -> msg) Decode.value)


clickTo : Route -> List Attribute
clickTo route =
  let
    path =
      encode route
  in
    [ href path
    , onWithOptions
      "click"
      { stopPropagation = True, preventDefault = True }
      Decode.value
      (\_ -> Signal.message TransitRouter.pushPathAddress path)
    ]
