import Platform.Cmd exposing (Cmd)
import Platform.Sub exposing (Sub)
import Html exposing (Html)
import Task exposing (Task)

import Auth
import Auth.Facebook as Facebook
import Auth.Google as Google
import World


port initialPath : String


app : Html.App World.Model
app =
  Html.App.program
    { init = World.init initialPath
    , update = World.update
    , view = World.view
    , subscriptions =
      [ World.actions
        { facebook = fbAuthResponse
        , google = gaResponse
        }
      ]
    }


main : Signal Html
main =
  app.html


port tasks : Signal (Task never ())
port tasks =
  app.tasks


-- Facebook integration
port fbAuthResponse : (Maybe Facebook.AuthResponse -> msg) -> Sub msg


port fbLogin : List String -> Cmd msg
port fbLogin =
  Facebook.loginRequests
  -- TODO: map the Facebook.login List Facebook.Scope to strings


port fbLogout : () -> Cmd msg
port fbLogout =
  Facebook.logoutRequests


-- Google integration
port gaResponse : Maybe Google.AuthResponse -> Cmd msg


port gaLogin : () -> Cmd msg
port gaLogin =
  Google.loginRequests


port gaLogout : () -> Cmd msg
port gaLogout =
  Google.logoutRequests


port gaContacts : () -> Cmd msg
port gaContacts =
  Google.contactsRequests


port trustoLogout : () -> Cmd msg
port trustoLogout =
  Auth.logoutSignal

-- Routing
