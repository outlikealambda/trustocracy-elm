module Main exposing (..)

import Html

import World

main : Program String
main =
  Html.programWithFlags
    { init = World.init
    , update = World.update
    , view = World.view
    , subscriptions = World.subscriptions
    }
