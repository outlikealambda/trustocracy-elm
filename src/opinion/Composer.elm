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
import Html.Events exposing (onClick)


type alias Composer =
  { opinion : Opinion
  , composerView : ComposerView
  }


type Action
  = FetchComplete Opinion
  | WriterMsg Writer.Action
  | SetView ComposerView


type ComposerView
  = Write
  | Preview


empty : Composer
empty =
  { opinion = Opinion.empty
  , composerView = Write
  }


init : User -> Topic -> (Composer, Effects Action)
init user topic =
  ( empty
  , Opinion.fetchByUserTopic user topic.id
    |> Effects.map FetchComplete
  )


update : Action -> Composer -> (Composer, Effects Action)
update action composer =
  case action of

    FetchComplete opinion ->
      ( { composer
        | opinion = Presenter.prepare <| Presenter.expand opinion
        }
      , Effects.none )

    SetView composerView ->
      ( { composer | composerView = composerView }
      , Effects.none
      )

    WriterMsg msg ->
      ( { composer
        | opinion = Writer.update msg composer.opinion
        }
      , Effects.none )


view : Signal.Address Action -> Composer -> Html
view address {opinion, composerView} =
  let content =
    case composerView of
      Write ->
        div
          [ class "writer" ]
          [ Writer.view (Signal.forwardTo address WriterMsg) opinion ]
      Preview ->
        div
          [ class "preview" ]
          [ Presenter.view opinion ]
  in
    div
      [ class "composer" ]
      [ composerNav address composerView
      , content
      ]


composerNav : Signal.Address Action -> ComposerView -> Html
composerNav address composerView =
  let
    (writeClasses, previewClasses) =
      case composerView of
        Write ->
          ("write-nav active", "preview-nav")
        Preview ->
          ("write-nav", "preview-nav active")
  in
    div
      [ class "composer-nav cf" ]
      [ div
        [ class writeClasses
        , onClick (Signal.forwardTo address SetView) Write
        ]
        [ text "Write" ]
      , div
        [ class previewClasses
        , onClick (Signal.forwardTo address SetView) Preview
        ]
        [ text "Preview" ]
      ]

navButton : Composer -> Html
navButton {opinion} =
  let
    actionText =
      if opinion.id == -1 then
        "Compose"
      else
        "Edit"
  in
    if opinion.fetched then
      div
        [ class "compose fetched" ]
        [ text actionText ]
    else
      div [] []
