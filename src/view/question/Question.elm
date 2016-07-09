module View.Question.Question exposing
  ( view
  )


import Model.Question.Answer as Answer exposing (Answer)
import Model.Question.Question as Question exposing (Question)
import View.Question.Picker as PickerView


import Html exposing (Html)


view : Question -> Maybe Answer -> Html Answer
view q mc =
  case q.selector of
    Question.Picker ->
      PickerView.view
        (Maybe.withDefault Answer.unanswered mc)
        q.options
        q.prompt
    Question.Rater ->
      Html.div [] []
