import Effects exposing (Never)
import Html exposing (Html)
import StartApp
import Task

import ActiveUser
import Auth.Facebook as Facebook
import User exposing (User)
import World


port initialPath : String
port activeUser : Maybe User


app : StartApp.App World.Model
app =
  StartApp.start
    { init = World.init initialPath (ActiveUser.fromMaybe activeUser)
    , update = World.update
    , view = World.view
    , inputs =
      [ World.actions
        { facebook = fbAuthResponses }
      ]
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
port fbAuthResponses : Signal (Maybe Facebook.AuthResponse)

port fbLogin : Signal (List String)
port fbLogin =
  Facebook.loginRequests
  -- TODO: map the Facebook.login List Facebook.Scope to strings

port fbLogout : Signal ()
port fbLogout =
  Facebook.logoutRequests
