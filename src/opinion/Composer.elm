module Opinion.Composer exposing
  ( Composer
  , Msg
  , empty
  , init
  , update
  , view
  , navButton
  )


import Common.API as API
import Opinion.Opinion as Opinion exposing (Opinion)
import Opinion.Writer as Writer
import Opinion.Presenter as Presenter
import Topic.Model as Topic exposing (Topic)
import Trustee
import User exposing (User)

import Platform.Cmd exposing (Cmd)
import Html exposing (Html, div, text, br)
import Html.Attributes exposing (class, placeholder, value)
import Html.Events exposing (on, onClick)
import Json.Decode as Decoder


type alias Composer =
  { opinion : Opinion
  , topic : Topic
  , composerView : ComposerView
  }


type Msg
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


-- This is a little ugly; we only need the User to create a new opinion
-- if there isn't an existing one.  But this means we need to import User and
-- Trustee, and removes the Maybe response logic from the Action handler...
-- TODO: create a new Opinion on the server side if the opinion doesn't exist?
init : User -> Topic -> (Composer, Cmd Msg)
init user topic =
  let
    emptyOpinion =
      Opinion.empty
    default =
      { emptyOpinion
      | opiner = Trustee.fromSelf user.name user.id
      , fetched = True
      }
  in
    ( { empty | topic = topic }
    , API.fetchDraftByTopic
      (FetchComplete << Maybe.withDefault default)
      topic.id
    )


update : Msg -> Composer -> (Composer, Cmd Msg)
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
      , API.saveOpinion WriteComplete composer.opinion composer.topic.id
      )

    Publish ->
      ( composer
      , API.publishOpinion WriteComplete composer.opinion composer.topic.id
      )


view : Composer -> Html Msg
view {opinion, composerView} =
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


composerNav : ComposerView -> Html Msg
composerNav composerView =
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
