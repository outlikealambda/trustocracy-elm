import Effects exposing (Never)
import World
import StartApp
import Task

import LocalStorage
import User exposing (User)


port initialPath : String
port activeUser : Maybe User


app =
  StartApp.start
    { init = World.init initialPath activeUser
    , update = World.update
    , view = World.view
    , inputs = [ World.actions ]
    }


main =
  app.html


port tasks : Signal (Task.Task Never ())
port tasks =
  app.tasks


port saveActiveUser : Signal (Maybe User)
port saveActiveUser =
  LocalStorage.saveActiveUserSignal
