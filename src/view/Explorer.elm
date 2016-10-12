module View.Explorer exposing
  ( connected
  , all
  , allButton
  , connectedButton
  )


import Model.Connection as Connection
import Model.Explorer exposing (Explorer)
import Model.Question.Question exposing (Question)

import Update.Explorer as Update

import View.Connection as ConnectionView


import Dict
import Html exposing (Html)
import Html.Attributes exposing (class)
import String


connected : Explorer -> Html Update.Msg
connected {connections, questions} =
  let
    viewConnection key connection =
      ConnectionView.connected (buildContext questions key) connection

  in
    Html.div
      []
      ( Dict.map viewConnection connections
        |> Dict.values
        |> List.filterMap identity
        |> List.intersperse (Html.hr [] [])
      )


all : Explorer -> Html Update.Msg
all {connections, questions} =
  let
    viewConnection key connection =
      ConnectionView.public (buildContext questions key) connection

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
    [ toString <| Connection.connectedCount <| Dict.values connections
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


buildContext : List Question -> Int -> ConnectionView.Context Update.Msg
buildContext questions connectionKey =
  { showAll = Update.Blur
  , readMore = Update.Focus connectionKey
  , next = Update.ConnectionMsg connectionKey
  , questions = questions
  }
