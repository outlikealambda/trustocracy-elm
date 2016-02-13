module Topic.View
  ( view
  , viewAll
  ) where


import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Html.Events


import Topic.Model exposing (Topic)


view : Topic -> Html
view topic =
  div [ class "topic" ]
    [ text topic.text ]


viewAll : List Topic -> Html
viewAll topics =
  div [ class "topics" ]
    (List.map view topics)
