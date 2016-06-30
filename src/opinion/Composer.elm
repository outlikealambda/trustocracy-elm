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
import Model.Topic as Topic exposing (Topic)
import Opinion.Writer as Writer
import View.Opinion as OpinionView

import Html exposing (Html)
import Html.App
import Html.Attributes exposing (class)
import Html.Events as Events


type alias Composer =
  { opinion : Opinion
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
  , composerView = Write
  }


init : Topic -> (Composer, Cmd Msg)
init topic =
  let
    emptyOpinion =
      Opinion.empty
    default =
      { emptyOpinion
      | fetched = True
      }
  in
    ( empty
    , API.fetchDraftByTopic
      FetchError
      FetchComplete
      topic.id
    )


update : Msg -> Topic -> Composer -> (Composer, Cmd Msg)
update action topic composer =
  case action of

    FetchComplete opinion ->
      { composer | opinion = opinion } ! []

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
          WriteError WriteComplete composer.opinion topic.id
        ]

    Publish ->
      composer
      ! [ API.publishOpinion
          WriteError WriteComplete composer.opinion topic.id
        ]

view : Composer -> Html Msg
view {opinion, composerView} =
  let content =
    case composerView of
      Write ->
        Html.App.map WriterMsg (Writer.view opinion)
      Preview ->
        Html.div
          [ class "preview" ]
          [ OpinionView.kitchenSink True opinion ]
  in
    Html.div
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
    Html.div
      [ class "composer-nav cf" ]
      [ Html.div
        [ class writeClasses
        , Events.onClick <| SetView Write
        ]
        [ Html.text "Write" ]
      , Html.div
        [ class previewClasses
        , Events.onClick <| SetView Preview
        ]
        [ Html.text "Preview" ]
      , Html.div
        [ class "publish-opinion"
        , Events.onClick Publish
        ]
        [ Html.text "Publish" ]
      , Html.div
        [ class "save-opinion"
        , Events.onClick Save
        ]
        [ Html.text "Save" ]
      ]

navButton : Composer -> Html m
navButton {opinion} =
  let
    actionText =
      if (Debug.log "Composer navButton opinion" opinion).id == -1 then
        [ Html.text "Compose"
        , Html.br [] []
        , Html.text "an"
        , Html.br [] []
        , Html.text "Opinion"
        ]
      else
        [ Html.text "Edit"
        , Html.br [] []
        , Html.text "your"
        , Html.br [] []
        , Html.text "Opinion"
        ]
  in
    if opinion.fetched then
      Html.div
        [ class "compose fetched" ]
        actionText
    else
      Html.div [] []
