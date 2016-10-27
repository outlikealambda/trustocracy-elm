module View.Header exposing
  ( view
  )


import Html exposing (Html)
import Html.Attributes exposing (class )
import Html.Events as Events


view : msg -> List (Html msg) -> Html msg
view msg sessionElements =
  let
    home =
      [ Html.div
        [ class "home" ]
        [ Html.i
          [ class "material-icons"
          , Events.onClick msg ]
          [ Html.text "home" ]
        ]
      ]

  in
    Html.div [ class "header" ]
      <| home
      ++ sessionElements
