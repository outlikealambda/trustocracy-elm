module Topic.View exposing
  ( view
  , viewAll
  )


import Html exposing (Html)
import Html.App
import Html.Attributes exposing (class)
import Html.Events as Events


import Model.Topic exposing (Topic)


type alias TopicId = Int


view : Topic -> Html TopicId
view topic =
  Html.div
    [ class "topic"
    , Events.onClick topic.id ]
    [ Html.text topic.text ]


viewAll : (TopicId -> msg) -> List Topic -> Html msg
viewAll transform topics =
  Html.div [ class "topics" ]
    (List.map (Html.App.map transform << view ) topics)
