module Utils.Cmd exposing (..)


import Task


init : a -> Cmd a
init msg =
  Task.succeed msg
  |> Task.perform identity identity
