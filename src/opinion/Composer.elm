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
import Model.Opinion.Composition as Composition exposing (Composition)
import Model.Topic as Topic exposing (Topic)
import Opinion.Writer as Writer
import View.Opinion as OpinionView

import Html exposing (Html)
import Html.Attributes exposing (class)
import Html.Events as Events


type alias Composer =
  { composition : Composition
  , composerView : ComposerView
  }


type Msg
  = FetchComplete Composition
  | FetchError String
  | WriteComplete Composition
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
  { composition = Composition.empty
  , composerView = Write
  }


init : Topic -> (Composer, Cmd Msg)
init topic =
  ( empty
  , API.fetchDraftByTopic
    FetchError
    FetchComplete
    topic.id
  )


update : Msg -> Topic -> Composer -> (Composer, Cmd Msg)
update action topic composer =
  case action of

    FetchComplete composition ->
      { composer | composition = composition } ! []

    FetchError err ->
      let
        msg = Debug.log "failed to fetch composed opinion" err
      in
        composer ! []

    WriteComplete composition ->
      let
        msg = Debug.log "written!" composition
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
      | composition = Writer.update msg composer.composition
      }
      ! []

    Save ->
      composer
      ! [ API.saveOpinion
          WriteError WriteComplete composer.composition topic.id
        ]

    Publish ->
      composer
      ! [ API.publishOpinion
          WriteError WriteComplete composer.composition topic.id
        ]

view : Composer -> Html Msg
view {composition, composerView} =
  let content =
    case composerView of
      Write ->
        Html.map WriterMsg (Writer.view composition)

      Preview ->
        Html.div
          [ class "preview" ]
          [ OpinionView.kitchenSink True composition ]

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
navButton {composition} =
  let
    actionText =
      case Composition.key composition of
        Nothing ->
          [ Html.text "Compose" ]
        Just _ ->
          [ Html.text "Edit" ]
  in
    if composition.fetched then
      Html.div
        [ class "compose fetched" ]
        actionText
    else
      Html.div [] []
