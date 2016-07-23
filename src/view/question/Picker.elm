module View.Question.Picker exposing
  ( view
  )


import Model.Question.Answer as Answer exposing (Answer)
import Model.Question.Option exposing (Option)


import Update.Question.Answer as AnswerUpdate


import Html exposing (Html)
import Html.App
import Html.Attributes as HtmlAttrs exposing (class)
import Html.Events as HtmlEvents


type alias Prompt = String


view : Answer -> List Option -> Prompt -> Html AnswerUpdate.Msg
view answer options prompt =
  Html.div
    [ class "picker cf" ]
    [ Html.div
      [ class "prompt" ]
      [ Html.text prompt ]
    , Html.ul
      [ class "options" ]
      ( List.map (radio answer) options )
    ]
    |> Html.App.map AnswerUpdate.Choose


radio : Answer -> Option -> Html Answer.Choice
radio answer {id, label} =
  let
    isSelected =
      case answer.choice of
        Answer.None ->
          False
        Answer.Picked optionId ->
          optionId == id
        _ ->
          False
  in
    Html.li
      [ HtmlAttrs.classList
        [ ("selected", isSelected)
        , ("option", True)
        ]
      , HtmlEvents.onClick <| Answer.Picked id
      ]
      [ Html.text label ]
