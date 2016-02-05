module World
  ( Action
  , init
  , view
  , update) where


import String
import Html exposing (Html, input, div, node, h1, text)
import Effects exposing (Effects)
import Html.Attributes exposing (class, rel, href, placeholder, value)
import Html.Events exposing (on, targetValue)


import Opinion.Connected as Connected
import Opinion.Composer as Composer
import User exposing (User)
import Topic exposing (Topic)


type alias Model =
  { user: User
  , topic: Topic
  , connected : Connected.Model
  , write : Composer.Model
  , view : View
  }


-- not used yet
type View
  = UsersConnectedOpinions
  | ComposeOpinion
  | UserInfo


type Action
  = SetUser Int
  | SwitchView View
  | ConnectedMsg Connected.Action
  | ComposerMsg Composer.Action


init : (Model, Effects Action)
init =
  let
    user =
      User "me" 0
    topic =
      0
    (connectedModel, connectedFx) =
      Connected.init user topic
    (writeModel, composerFx) =
      Composer.init user topic
  in
    ( Model user topic connectedModel writeModel UsersConnectedOpinions
    , Effects.batch
      [ Effects.map ConnectedMsg connectedFx
      , Effects.map ComposerMsg composerFx
      ]
    )


update : Action -> Model -> (Model, Effects Action)
update message model =
  case message of
    SetUser uid ->
      let
          updatedUser = User ("me" ++ (toString uid)) uid
          (connectedModel, fx) = Connected.update (Connected.SetUser updatedUser) model.connected
      in
        ( { model
          | user = updatedUser
          , connected = connectedModel
          }
        , Effects.map ConnectedMsg fx
        )

    SwitchView view ->
      ( { model | view = view }
      , Effects.none
      )

    ConnectedMsg msg ->
      let
          (connectedModel, fx) = Connected.update msg model.connected
      in
        ( { model | connected = connectedModel }
        , Effects.map ConnectedMsg fx
        )

    ComposerMsg msg ->
      let
          (writeModel, fx) = Composer.update msg model.write
      in
          ( { model | write = writeModel }
          , Effects.map ComposerMsg fx
          )


view : Signal.Address Action -> Model -> Html
view address model =
  let field =
        input
          [ placeholder "User ID"
          , value <| toString model.user.id
          , on "input" targetValue (Signal.message address << SetUser << processStr)
          ]
          []

      connectedGroups =
        Connected.view (Signal.forwardTo address ConnectedMsg) model.connected

      write =
        Composer.view (Signal.forwardTo address ComposerMsg) model.write
  in
      div [ class "world container" ]
        [ css "css/normalize.css"
        , css "css/fonts.css"
        , css "css/trusto.css"
        , h1 [] [ text model.user.name ]
        , field
        , write
        , div [class "row"] connectedGroups
        ]

-- todo: handle errors better, although we shouldn't be getting invalid users
-- here eventually
processStr : String -> Int
processStr raw = String.toInt raw |> Result.withDefault 0

css : String -> Html
css path =
  node "link" [rel "stylesheet", href path] []
