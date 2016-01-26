module OpinionCreate
  ( Model
  , Action
  , update
  , viewCreator
  , viewCredentialsInput
  ) where


import String
import Html exposing (Html, div, textarea, text)
import Html.Attributes exposing (class, placeholder, value)
import Html.Events exposing (on, targetValue)
import Effects exposing (Effects)
import Json.Decode as Json exposing ((:=))


import Credentials
import Opinion


type alias Model = Opinion.Model


type Action
  = Write String
  | CredentialsMsg Credentials.Action


update : Action -> Model -> (Model, Effects Action)
update message model =
  case message of
    Write rawText ->
      ( { model | text = rawText }
      , Effects.none
      )

    CredentialsMsg msg ->
      ( { model | credentials = Credentials.update msg model.credentials}
      , Effects.none )


viewCreator : Signal.Address Action -> Model -> Html
viewCreator address model =
  div [ class "opinion-creator" ]
    [ div [ class "input-field" ]
      [ textarea
        [ class "write"
        , placeholder "Let's write something!"
        , value model.text
        , on "input" targetValue (Signal.message address << Write)
        ]
        []
      ]
    , div [ class "character-count" ]
      [ String.length model.text
        |> toString
        |> flip (++) " characters written"
        |> text
      ]
    ]


viewCredentialsInput : Signal.Address Action -> Model -> Html
viewCredentialsInput address model =
  Credentials.viewForm (Signal.forwardTo address CredentialsMsg) model.credentials
