module View.Question.Picker exposing
  ( view
  )


import Model.Question.Chosen as Chosen exposing (Chosen)
import Model.Question.Option exposing (Option)


import Html exposing (Html)
import Html.Attributes as HtmlAttrs exposing (class)
import Html.Events as HtmlEvents


type alias Prompt = String


view : Chosen -> List Option -> Prompt -> Html Chosen
view chosen options prompt =
  let
    radio' =
      radio chosen
  in
    Html.div
      [ class "picker cf" ]
      ( [ Html.div
          [ class "prompt" ]
          [ Html.text prompt ]
        ]
        ++ ( List.map radio' options )
      )


radio : Chosen -> Option -> Html Chosen
radio chosen {id, label} =
  let
    isSelected =
      case chosen of
        Chosen.None ->
          False
        Chosen.Picked optionId ->
          optionId == id
        _ ->
          False
  in
    Html.div
      [ HtmlAttrs.classList
        [ ("selected", isSelected)
        , ("option", True)
        ]
      , HtmlEvents.onClick <| Chosen.Picked id
      ]
      [ Html.text label ]
