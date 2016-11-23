module Main exposing (..)

import Html

import World exposing (World)

main : Program String World World.Msg
main =
  Html.programWithFlags
    { init = World.init
    , update = World.update
    , view = World.view
    , subscriptions = World.subscriptions
    }
