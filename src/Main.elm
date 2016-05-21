module Main exposing (..)

import Html.App

import World

main : Program String
main =
  Html.App.programWithFlags
    { init = World.init
    , update = World.update
    , view = World.view
    , subscriptions = World.subscriptions
    }
