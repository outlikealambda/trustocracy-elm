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
import Model.Opinion as Opinion exposing (Opinion)
import Model.Trustee as Trustee
import Opinion.Writer as Writer
import Opinion.Presenter as Presenter
import Topic.Model as Topic exposing (Topic)
import User exposing (User)

import Html exposing (Html, div, text, br)
import Html.App
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
  | FetchError String
  | WriteComplete Opinion
  | WriteError String
  | Save
  | Publish
  | WriterMsg Writer.Msg
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
      FetchError
      FetchComplete
      topic.id
    )


update : Msg -> Composer -> (Composer, Cmd Msg)
update action composer =
  case action of

    FetchComplete opinion ->
      { composer | opinion = prepOpinion opinion } ! []

    FetchError err ->
      let
        msg = Debug.log "failed to fetch composed opinion" err
      in
        composer ! []

    WriteComplete opinion ->
      let
        msg = Debug.log "written!" opinion
      in
        composer ! []

    WriteError err ->
      let
        msg = Debug.log "failed to write!" err
      in
        composer ! []

    SetView composerView ->
      { composer | composerView = composerView }
      ! []

    WriterMsg msg ->
      { composer
      | opinion = Writer.update msg composer.opinion
      }
      ! []

    Save ->
      composer
      ! [ API.saveOpinion
          WriteError WriteComplete composer.opinion composer.topic.id
        ]

    Publish ->
      composer
      ! [ API.publishOpinion
          WriteError WriteComplete composer.opinion composer.topic.id
        ]

prepOpinion : Opinion -> Opinion
prepOpinion opinion =
  { opinion | fetched = True }
  |> Presenter.prepare
  |> Presenter.expand


view : Composer -> Html Msg
view {opinion, composerView} =
  let content =
    case composerView of
      Write ->
        Html.App.map WriterMsg (Writer.view opinion)
      Preview ->
        div
          [ class "preview" ]
          [ Presenter.viewExpanded opinion ]
  in
    div
      [ class "composer" ]
      [ composerNav composerView
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
      [ Html.App.map SetView <| div
        [ class writeClasses
        , onClick Write
        ]
        [ text "Write" ]
      , Html.App.map SetView <| div
        [ class previewClasses
        , onClick Preview
        ]
        [ text "Preview" ]
      , div
        [ class "publish-opinion"
        , on "click" (Decoder.map (\_ -> Publish) Decoder.value)
        ]
        [ text "Publish" ]
      , div
        [ class "save-opinion"
        , on "click" (Decoder.map (\_ -> Debug.log "clicked" Save) Decoder.value)
        ]
        [ text "Save" ]
      ]

navButton : Composer -> Html m
navButton {opinion} =
  let
    actionText =
      if (Debug.log "Composer navButton opinion" opinion).id == -1 then
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
