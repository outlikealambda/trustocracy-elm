module Topic.View
  ( view
  , viewAll
  ) where


import Html exposing (Html, div, text)
import Html.Attributes exposing (class, href)
import Html.Events exposing (onWithOptions)
import Json.Decode as Json


import Topic.Model exposing (Topic)
import Routes


import TransitRouter


view : Topic -> Html
view topic =
  div
    [ class "topic", clickTo <| Routes.encode (Routes.Survey topic.id) ]
    [ text topic.text ]


viewAll : List Topic -> Html
viewAll topics =
  div [ class "topics" ]
    (List.map view topics)


clickTo : String -> Html.Attribute
clickTo path =
  onWithOptions
    "click"
    { stopPropagation = True, preventDefault = True }
    Json.value
    (\_ -> Signal.message TransitRouter.pushPathAddress path)
