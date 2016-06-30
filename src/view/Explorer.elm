module View.Explorer exposing
  ( view
  , navButton
  )


import Model.Explorer exposing (Explorer)
import Update.Explorer as Update
import View.Connection as ConnectionView


import Dict
import Html exposing (Html)
import Html.Attributes exposing (class)


view : (Update.Msg -> msg) -> Explorer -> Html msg
view transform { connections } =
  let
    context =
      ConnectionView.Context
        (transform << Update.Blur)
        (transform << Update.Focus)

  in
    Html.div
      []
      ( Dict.values connections
        |> List.sortBy .score
        |> List.map (ConnectionView.view context)
      )

navButton : Explorer -> Html msg
navButton {connections} =

  let
    count = Dict.size connections
  in
    Html.div
      [ class "connect fetched" ]
      [ Html.text <| (toString count) ++ " Opinions" ]
