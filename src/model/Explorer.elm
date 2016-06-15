module Model.Explorer exposing
  ( Explorer
  )


import Model.Connection as Connection exposing (Connection)
import Model.Expandable as Expandable
import View.Connection as ConnectionView


import Dict exposing (Dict)
import Html exposing (Html)


type alias Explorer =
  { connections : Dict Key Connection
  }


type alias Key = Int


type Msg
  = Focus Key
  | Blur


update : Msg -> Explorer -> (Explorer, Cmd Msg)
update message explorer =
  case message of
    Focus key ->
      let
        blurred =
          Dict.map (\_ -> Expandable.collapse) explorer.connections
        focused =
          Dict.update key (Maybe.map Expandable.expand) blurred
      in
        { explorer | connections = focused } ! []

    Blur ->
      let blurred =
        Dict.map (\_ -> Expandable.collapse) explorer.connections
      in
        { explorer | connections = blurred } ! []


view : Explorer -> Html msg
view { connections } =
  Html.div
    []
    ( Dict.values connections
      |> List.map ConnectionView.view
    )
