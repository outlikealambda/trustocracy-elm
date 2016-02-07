import Effects exposing (Never)
import World
import StartApp
import Task


port initialPath : String


app =
  StartApp.start
    { init = World.init initialPath
    , update = World.update
    , view = World.view
    , inputs = [ World.actions ]
    }


main =
  app.html


port tasks : Signal (Task.Task Never ())
port tasks =
  app.tasks
