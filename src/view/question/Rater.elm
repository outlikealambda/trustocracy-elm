module View.Question.Rater exposing
  ( view
  )


import Common.Tether as Tether


import Model.Question.Answer as Answer exposing (Answer, Choice)
import Model.Question.Option as Option exposing (Option)


import Update.Question.Answer as AnswerUpdate


import Html exposing (Html, Attribute)
import Html.App
import Html.Attributes as HtmlAttrs exposing (class)
import Html.Events as HtmlEvents
import Json.Decode as Decode
import String


type alias Prompt = String


view : Answer -> (Option, Option) -> Prompt -> Html AnswerUpdate.Msg
view answer (leftEndpoint, rightEndpoint) prompt =
  Html.div
    [ class "rater cf" ]
    ( [ Html.div
        [ class "prompt" ]
        [ Html.text prompt ]
      , Html.div
        [ class "slider"]
        [ Html.input
          [ HtmlAttrs.type' "range"
          , HtmlAttrs.min "0"
          , HtmlAttrs.max "1"
          , HtmlAttrs.step ".01"
          , HtmlAttrs.value <| toString <| getRating answer
          , mouseUpWithDefault 0.5
          ]
          []
        , Html.div
          [ class "labels" ]
          [ Html.div
            [ class "endpoint-left"]
            [ Html.text leftEndpoint.label ]
          , Html.div
            [ class "endpoint-right"]
            [ Html.text rightEndpoint.label ]
          ]
        ]
      ]
    )
  |> Html.App.map AnswerUpdate.Choose


mouseUpWithDefault : Float -> Attribute Choice
mouseUpWithDefault default =
  HtmlEvents.on
    "mouseup"
    ( Decode.map
      (\v ->
        Answer.Rated <| Result.withDefault default (String.toFloat v)
      )
      ( Decode.at ["target", "value"] Decode.string )
    )


getRating : Answer -> Float
getRating answer =
  let
    choice =
      Tether.data answer
  in
    case choice of
      Answer.Rated v ->
        v
      _ ->
        0.5
