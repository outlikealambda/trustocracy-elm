module Opinion.Composer
  ( Composer
  , Action
  , empty
  , init
  , update
  , view
  , navButton
  ) where


import Opinion.Opinion as Opinion exposing (Opinion)
import Opinion.Writer as Writer
import Opinion.Presenter as Presenter
import User exposing (User)
import Topic.Model exposing (Topic)

import Effects exposing (Effects)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, placeholder, value)


type alias Composer = Opinion


type Action
  = FetchComplete Opinion
  | WriterMsg Writer.Action


empty : Composer
empty = Opinion.empty


init : User -> Topic -> (Composer, Effects Action)
init user topic =
  ( { empty | user = user }
  , Opinion.fetchByUserTopic user topic.id
    |> Effects.map FetchComplete
  )


update : Action -> Composer -> (Composer, Effects Action)
update action composer =
  case action of

    FetchComplete opinion ->
      ( (Presenter.prepare << Presenter.expand) opinion
      , Effects.none )

    WriterMsg msg ->
      ( Debug.log "write update" <| Writer.update msg composer
      , Effects.none )


view : Signal.Address Action -> Composer -> Html
view address composer =
  div [ class "row composer" ]
    [ div [ class "col m12 l6" ]
      [ div [ class "t-card" ]
        [ div [ class "t-card-body" ]
          [ div [ class "subtitle" ] [ text "Write" ]
          , Writer.view (Signal.forwardTo address WriterMsg) composer
          ]
        ]
      ]
    , div [ class "col m12 l6 preview" ]
      [ div [ class "t-card" ]
        [ div [ class "t-card-body" ]
          [ div [ class "subtitle" ] [ text "Preview" ]
          , Presenter.view composer
          ]
        ]
      ]
    ]

navButton : Composer -> Html
navButton {id, fetched} =
  let
    actionText =
      if id == -1 then
        "Compose"
      else
        "Edit"
  in
    if fetched then
      div
        [ class "compose fetched" ]
        [ text actionText ]
    else
      div [] []
