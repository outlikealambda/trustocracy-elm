module View.Explorer exposing
  ( exploreButton
  , connectedButton
  , view
  )


import Model.TopicOpinion.Connection as Connection
import Model.Explorer as Explorer exposing (Explorer)
import Model.Question.Question exposing (Question)

import Update.Explorer as Update

import View.Connection as ConnectionView
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
focused oid {connections, questions, assessor} =
  let
    context =
      buildContext True questions oid

    assessorView =
      AssessorView.questions questions assessor
      |> Html.App.map (Update.DelegateToAssessor oid)

    combineViews connectionView =
      Html.div
        [ class "explorer focused" ]
        [ connectionView
        , assessorView
        ]
  in
    Dict.get oid connections
    |> Maybe.map (ConnectionView.view context)
    |> Maybe.map combineViews
    |> Maybe.withDefault (Html.div [] [])


blurred : Explorer -> Html Update.Msg
blurred explorer =
  let
    contextBuilder =
      buildContext False explorer.questions

    viewConnection connection =
      let
        context =
          contextBuilder <| Connection.key connection
      in
        ConnectionView.view context connection

  in
    Html.div
      [ class "explorer blurred" ]

      ( Explorer.sortConnections explorer
        |> List.map viewConnection
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
connectedButton {connections} =
  String.join " "
    [ toString <| Connection.countLinked <| Dict.values connections
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


buildContext : Bool -> List Question -> Int -> ConnectionView.Context Update.Msg
buildContext isExpanded questions connectionKey =
  { showAll = Update.Blur
  , readMore = \_ -> Update.Focus connectionKey
  , questions = questions
  , isExpanded = isExpanded
  }
