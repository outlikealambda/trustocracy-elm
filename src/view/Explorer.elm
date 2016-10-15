module View.Explorer exposing
  ( connected
  , all
  , allButton
  , connectedButton
  , view
  )


import Model.Connection.Connection as Connection
import Model.Explorer as Explorer exposing (Explorer)
import Model.Question.Question exposing (Question)

import Update.Explorer as Update

import View.Connection as ConnectionView
import View.Question.Assessor as AssessorView


import Dict
import Html exposing (Html)
import Html.Attributes exposing (class)
import Html.App
import String


connected : Explorer -> Html Update.Msg
connected {connections, questions} =
  let
    viewConnection key connection =
      ConnectionView.view (buildContext False questions key) connection

  in
    Html.div
      []
      ( Dict.map viewConnection connections
        |> Dict.values
        |> List.intersperse (Html.hr [] [])
      )


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
blurred {connections, questions} =
  let
    viewConnection key connection =
      ConnectionView.view (buildContext False questions key) connection

  in
    Html.div
      [ class "explorer blurred" ]
      ( Dict.map viewConnection connections
        |> Dict.values
        |> List.intersperse (Html.hr [] [])
      )

all : Explorer -> Html Update.Msg
all {connections, questions} =
  let
    viewConnection key connection =
      ConnectionView.view (buildContext False questions key) connection

  in
    Html.div
      []
      ( Dict.map viewConnection connections
        |> Dict.values
        |> List.intersperse (Html.hr [] [])
      )


connectedButton : Explorer -> Html msg
connectedButton {connections} =
  String.join " "
    [ toString <| Connection.countLinked <| Dict.values connections
    , "Linked Opinions"
    ]
    |> navButton


allButton : Explorer -> Html msg
allButton {connections} =
  "Explore All Opinions"
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
