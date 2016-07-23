module View.Explorer exposing
  ( view
  , Context
  , navButton
  )


import Model.Question.Question exposing (Question)
import Model.Connection as Connection
import Model.Explorer exposing (Explorer)
import Update.Explorer as Update
import View.Connection as ConnectionView


import Dict
import Html exposing (Html)
import Html.Attributes exposing (class)


type alias Context = { topicId : Int }


view : Context -> Explorer -> Html Update.Msg
view {topicId} {connections, questions} =
  let
    viewConnection c =
      ConnectionView.connection (buildContext questions <| Connection.key c) c

  in
    Html.div
      []
      ( Dict.values connections
        |> List.sortBy .score
        |> List.map viewConnection
        |> List.intersperse (Html.hr [] [])
      )

navButton : Explorer -> Html msg
navButton {connections} =
  let
    count = Dict.size connections

  in
    Html.div
      [ class "connect fetched" ]
      [ Html.text <| (toString count) ++ " Opinions" ]



buildContext : List Question -> Int -> ConnectionView.Context Update.Msg
buildContext questions connectionKey =
  { showAll = Update.Blur
  , readMore = Update.Focus
  , next = Update.ConnectionMsg connectionKey
  , questions = questions
  }
