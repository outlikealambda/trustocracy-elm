import Effects exposing (Never)
import Html exposing (Html)
import StartApp
import Task

import Auth
import Auth.Facebook as Facebook
import Auth.Google as Google
import World


port initialPath : String


app : StartApp.App World.Model
app =
  StartApp.start
    { init = World.init initialPath
    , update = World.update
    , view = World.view
    , inputs =
      [ World.actions
        { facebook = fbAuthResponse
        , google = gaResponse
        }
      ]
    }


main : Signal Html
main =
  app.html


port tasks : Signal (Task.Task Never ())
port tasks =
  app.tasks


-- Facebook integration
port fbAuthResponse : Signal (Maybe Facebook.AuthResponse)


port fbLogin : Signal (List String)
port fbLogin =
  Facebook.loginRequests
  -- TODO: map the Facebook.login List Facebook.Scope to strings


port fbLogout : Signal ()
port fbLogout =
  Facebook.logoutRequests


-- Google integration
port gaResponse : Signal (Maybe Google.AuthResponse)


port gaLogin : Signal ()
port gaLogin =
  Google.loginRequests


port gaLogout : Signal ()
port gaLogout =
  Google.logoutRequests


port trustoLogout : Signal ()
port trustoLogout =
  Auth.logoutSignal
