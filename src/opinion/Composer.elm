module Opinion.Composer
  ( Model
  , empty
  , init
  , Action
  , update
  , view
  ) where


import Html exposing (Html, input, div, textarea, text, h3, button)
import Html.Attributes exposing (class, placeholder, value)
import Effects exposing (Effects)


import Opinion.Model as Opinion
import Opinion.View as View
import Opinion.Create as Create
import User exposing (User)
import Topic.Model exposing (Topic)


type alias Model =
  { opinion : Create.Model
  }


empty : Model
empty = Model Opinion.empty


init : User -> Topic -> (Model, Effects Action)
init user topic =
  let
    ( opinion, fx ) =
      Create.init user.id topic.id
  in
    ( Model opinion
    , Effects.map CreateMsg fx
    )


type Action
  = CreateMsg Create.Action
  | Publish


update : Action -> Model -> (Model, Effects Action)
update message model =
  case message of

    CreateMsg msg ->
      let
        (updatedOpinion, fx) =
          Create.update msg model.opinion
      in
        ( { model | opinion = updatedOpinion }
        , Effects.map CreateMsg fx
        )

    Publish ->
      ( model
      , Effects.none
      )


view : Signal.Address Action -> Model -> Html
view address model =
  div [ class "row composer" ]
    [ div [ class "col m12 l6" ]
      [ div [ class "t-card" ]
        [ div [ class "t-card-body" ]
          [ div [ class "subtitle" ] [ text "Write" ]
          , Create.viewCreator (Signal.forwardTo address CreateMsg) model.opinion
          ]
        ]
      , div [ class "t-card" ]
        [ div [ class "t-card-body" ]
          [ Create.viewCredentialsInput (Signal.forwardTo address CreateMsg) model.opinion ]
        ]
      ]
    , div [ class "col m12 l6 preview" ]
      [ div [ class "t-card" ]
        [ div [ class "t-card-body" ]
          [ div [ class "subtitle" ] [ text "Preview" ]
          , View.view model.opinion
          ]
        ]
      , button [ class "publish forward-action"] [ text "publish" ]
      ]
    ]
