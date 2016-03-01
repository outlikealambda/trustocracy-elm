import Effects exposing (Never)
import Html exposing (Html)
import World
import StartApp
import Task

import ActiveUser
import User exposing (User)
import Auth.Facebook as Facebook


port initialPath : String
port activeUser : Maybe User


app : StartApp.App World.Model
app =
  StartApp.start
    { init = World.init initialPath (ActiveUser.fromMaybe activeUser)
    , update = World.update
    , view = World.view
    , inputs = [ World.actions fbAuth ]
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


-- Facebook integration
port fbAuth : Signal Facebook.LoginStatusResponse
port fbUser : Signal (Maybe Facebook.UserResponse)

port fbAuthRequest : Signal String
port fbAuthRequest =
  Signal.map Facebook.toString Facebook.signal
