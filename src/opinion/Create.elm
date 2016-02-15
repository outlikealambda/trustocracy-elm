module Opinion.Create
  ( Model
  , init
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


import Opinion.Credentials as Credentials
import Opinion.Model as Opinion


type alias Model = Opinion.Model


init : Int -> Int -> (Model, Effects Action)
init userId topicId =
  ( Opinion.empty
  , Opinion.fetchByUserTopic userId topicId
    |> Effects.map Init
  )


type Action
  = Init Model
  | Write String
  | CredentialsMsg Credentials.Action


update : Action -> Model -> (Model, Effects Action)
update message model =
  case message of
    Init retrieved ->
      ( Opinion.setExpanded retrieved
      , Effects.none
      )

    Write rawText ->
      ( Opinion.setText rawText model
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
