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
import Topic.Model as Topic exposing (Topic)

import Effects exposing (Effects)
import Html exposing (Html, div, text, br)
import Html.Attributes exposing (class, placeholder, value)
import Html.Events exposing (on, onClick)
import Json.Decode as Decoder


type alias Composer =
  { opinion : Opinion
  , topic : Topic
  , composerView : ComposerView
  }


type Action
  = FetchComplete Opinion
  | WriteComplete (Maybe Opinion)
  | Save
  | Publish
  | WriterMsg Writer.Action
  | SetView ComposerView


type ComposerView
  = Write
  | Preview


empty : Composer
empty =
  { opinion = Opinion.empty
  , topic = Topic.empty
  , composerView = Write
  }


init : User -> Topic -> (Composer, Effects Action)
init user topic =
  ( { empty | topic = topic }
  , Opinion.fetchDraftByUserTopic user topic.id
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

    WriteComplete maybeOpinion ->
      case Debug.log "written!" maybeOpinion of
        Nothing ->
          ( composer, Effects.none )
        Just opinion ->
          ( composer, Effects.none )

    SetView composerView ->
      ( { composer | composerView = composerView }
      , Effects.none
      )

    WriterMsg msg ->
      ( { composer
        | opinion = Writer.update msg composer.opinion
        }
      , Effects.none )

    Save ->
      ( composer
      , Effects.map WriteComplete (Opinion.save composer.opinion composer.topic.id)
      )

    Publish ->
      ( composer
      , Effects.map WriteComplete (Opinion.publish composer.opinion composer.topic.id)
      )


view : Signal.Address Action -> Composer -> Html
view address {opinion, composerView} =
  let content =
    case composerView of
      Write ->
        Writer.view (Signal.forwardTo address WriterMsg) opinion
      Preview ->
        div
          [ class "preview" ]
          [ Presenter.viewExpanded opinion ]
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
      , div
        [ class "publish-opinion"
        , on "click" Decoder.value (\_ -> Signal.message address Publish)
        ]
        [ text "Publish" ]
      , div
        [ class "save-opinion"
        , on "click" Decoder.value (\_ -> Signal.message address <| Debug.log "clicked" Save)
        ]
        [ text "Save" ]
      ]

navButton : Composer -> Html
navButton {opinion} =
  let
    actionText =
      if opinion.id == -1 then
        [ text "Compose"
        , br [] []
        , text "an"
        , br [] []
        , text "Opinion"
        ]
      else
        [ text "Edit"
        , br [] []
        , text "your"
        , br [] []
        , text "Opinion"
        ]
  in
    if opinion.fetched then
      div
        [ class "compose fetched" ]
        actionText
    else
      div [] []
