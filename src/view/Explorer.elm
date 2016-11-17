module View.Explorer exposing
  ( exploreButton
  , connectedButton
  , view
  )


import Model.TopicOpinion.TopicOpinion as TopicOpinion
import Model.Explorer as Explorer exposing (Explorer)
import Model.Question.Question exposing (Question)

import Update.Explorer as Update

import View.TopicOpinion as TopicOpinionView
import View.Question.Assessor as AssessorView


import Dict
import Html exposing (Html)
import Html.Attributes exposing (class)
import Html.App
import Html.Events as Events
import String


view : Explorer -> Html Update.Msg
view explorer =
  case explorer.zoom of
    Explorer.Blurred ->
      blurred explorer

    Explorer.Focused oid ->
      focused oid explorer


focused : Int -> Explorer -> Html Update.Msg
focused oid {topicOpinions, questions, assessor} =
  let
    context =
      buildContext True questions oid

    assessorView =
      AssessorView.questions questions assessor
      |> Html.App.map (Update.DelegateToAssessor oid)

    combineViews topicOpinionView =
      Html.div
        [ class "explorer focused" ]
        [ topicOpinionView
        , assessorView
        ]
  in
    Dict.get oid topicOpinions
    |> Maybe.map (TopicOpinionView.view context)
    |> Maybe.map combineViews
    |> Maybe.withDefault (Html.div [] [])


blurred : Explorer -> Html Update.Msg
blurred explorer =
  let
    contextBuilder =
      buildContext False explorer.questions

    viewTopicOpinion topicOpinion =
      let
        context =
          contextBuilder <| TopicOpinion.key topicOpinion
      in
        TopicOpinionView.view context topicOpinion

  in
    Html.div
      [ class "explorer blurred" ]

      ( Explorer.sortTopicOpinions explorer
        |> List.map viewTopicOpinion
        |> List.intersperse (Html.hr [] [])
        |> (::) (sortButton explorer)
      )


sortButton : Explorer -> Html Update.Msg
sortButton {sort} =
  Html.div
    [ class <| "sort-button " ++ Explorer.classifySort sort
    , Events.onClick Update.NextSort ]
    [ Html.text "By Influence"]


connectedButton : Explorer -> Html msg
connectedButton {topicOpinions} =
  String.join " "
    [ toString <| TopicOpinion.countLinked <| Dict.values topicOpinions
    , "Linked Opinions"
    ]
    |> navButton


exploreButton : Explorer -> Html msg
exploreButton ignored =
  "Explore"
    |> navButton


navButton : String -> Html msg
navButton label =
  Html.div
    [ class "fetched" ]
    [ Html.text label ]


buildContext : Bool -> List Question -> Int -> TopicOpinionView.Context Update.Msg
buildContext isExpanded questions topicOpinionKey =
  { showAll = Update.Blur
  , readMore = \_ -> Update.Focus topicOpinionKey
  , questions = questions
  , isExpanded = isExpanded
  }
