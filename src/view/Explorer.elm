module View.Explorer exposing
  ( view )


import Model.Explorer exposing (Explorer)
import Update.Explorer as Update
import View.Connection as ConnectionView


import Dict
import Html exposing (Html)


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
        |> List.map (ConnectionView.view context)
      )
