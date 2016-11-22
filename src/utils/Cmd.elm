module Utils.Cmd exposing (..)


import Process
import Task exposing (Task)
import Time exposing (Time)


init : a -> Cmd a
init msg =
  Task.succeed msg
    |> Task.perform identity


delay : Time -> msg -> Cmd msg
delay time msg =
  Process.sleep time
    |> Task.perform (always msg)


mapCmdPair : (messageA -> messageB) -> (a, Cmd messageA) -> (a, Cmd messageB)
mapCmdPair transform (model, cmd) =
  (model, Cmd.map transform cmd)
