import Effects exposing (Never)
import Html exposing (Html)
import World
import StartApp
import Task

import ActiveUser
import User exposing (User)


port initialPath : String
port activeUser : Maybe User


app : StartApp.App World.Model
app =
  StartApp.start
    { init = World.init initialPath (ActiveUser.fromMaybe activeUser)
    , update = World.update
    , view = World.view
    , inputs = [ World.actions ]
    }


main : Signal Html
main =
  app.html


port tasks : Signal (Task.Task Never ())
port tasks =
  app.tasks


port saveActiveUser : Signal (Maybe User)
port saveActiveUser =
  Signal.map ActiveUser.toMaybe ActiveUser.signal
