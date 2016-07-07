module View.Question.Question exposing
  ( view
  )


import Model.Question.Chosen as Chosen exposing (Chosen)
import Model.Question.Question as Question exposing (Question)
import View.Question.Picker as PickerView


import Html exposing (Html)


view : Question -> Maybe Chosen -> Html Chosen
view q mc =
  case q.selector of
    Question.Picker ->
      PickerView.view
        (Maybe.withDefault Chosen.None mc)
        q.options
        q.prompt
    Question.Rater ->
      Html.div [] []
