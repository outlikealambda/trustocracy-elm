module Topic.View exposing
  ( view
  , viewAll
  )


import Html exposing (Html, div, text)
import Html.Attributes exposing (class, href)
import Html.Events exposing (onWithOptions)
import Json.Decode as Json


import Topic.Model exposing (Topic)
import Routes


view : (Routes.Route -> msg) -> Topic -> Html msg
view onRoute topic =
  div
    [ class "topic", clickTo <| onRoute (Routes.Survey topic.id) ]
    [ text topic.text ]


viewAll : (Routes.Route -> msg) -> List Topic -> Html msg
viewAll onRoute topics =
  div [ class "topics" ]
    (List.map (view onRoute) topics)


clickTo : msg -> Html.Attribute msg
clickTo msg =
  onWithOptions
    "click"
    { stopPropagation = True, preventDefault = True }
    (Json.map (\_ -> msg) Json.value)
