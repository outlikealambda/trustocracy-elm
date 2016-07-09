module View.Explorer exposing
  ( view
  , Context
  , navButton
  )


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
    context =
      ConnectionView.Context
        Update.Blur
        Update.Focus
        Update.ConnectionMsg
        questions
        topicId


  in
    Html.div
      []
      ( Dict.values connections
        |> List.sortBy .score
        |> List.map (ConnectionView.view context)
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
