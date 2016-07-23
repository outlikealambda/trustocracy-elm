module View.Question.Question exposing
  ( view
  )


import Model.Question.Answer as Answer exposing (Answer)
import Model.Question.Question as Question exposing (Question)
import View.Question.Picker as PickerView
import View.Question.Rater as RaterView


import Html exposing (Html)


view : Question -> Maybe Answer -> Html Answer
view q maybeAnswer =
  let
    answer =
      Maybe.withDefault Answer.unanswered maybeAnswer

  in
    case q.selector of

      Question.Picker options ->
        PickerView.view answer options q.prompt

      Question.Rater endpoints ->
        RaterView.view answer endpoints q.prompt
