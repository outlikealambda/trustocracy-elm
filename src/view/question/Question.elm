module View.Question.Question exposing
  ( view
  )


import Model.Question.Question as Question exposing (Question)
import Update.Question.Question as Update
import View.Question.Picker as PickerView


import Html exposing (Html)
import Html.App


view : Question -> Html Update.Msg
view q =
  case q.selector of
    Question.Picker ->
      PickerView.view q.chosen q.options q.prompt
      |> Html.App.map Update.ChosenMsg
    Question.Rater ->
      Html.div [] []
