module Header exposing
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
        [ Html.a
          [ Events.onClick msg ]
          [ Html.text "Home" ]
        ]
      ]

  in
    Html.div [ class "header" ]
      <| home
      ++ sessionElements
