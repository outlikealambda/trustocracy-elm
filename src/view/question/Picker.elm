module View.Question.Picker exposing
  ( view
  )


import Model.Question.Answer as Answer exposing (Answer)
import Model.Question.Option exposing (Option)


import Html exposing (Html)
import Html.Attributes as HtmlAttrs exposing (class)
import Html.Events as HtmlEvents


type alias Prompt = String


view : Answer -> List Option -> Prompt -> Html Answer
view answer options prompt =
  let
    radio' =
      radio answer
  in
    Html.div
      [ class "picker cf" ]
      ( [ Html.div
          [ class "prompt" ]
          [ Html.text prompt ]
        ]
        ++ ( List.map radio' options )
      )


radio : Answer -> Option -> Html Answer
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
    Html.div
      [ HtmlAttrs.classList
        [ ("selected", isSelected)
        , ("option", True)
        ]
      , HtmlEvents.onClick <| { answer | choice = Answer.Picked id }
      ]
      [ Html.text label ]
