module View.Topic exposing
  ( all
  )


import Model.Topic exposing (Topic)
import Utils.Date as DateUtils


import Html exposing (Html)
import Html.Attributes exposing (class)
import Html.Events as Events


type alias TopicId = Int


view : Topic -> Html TopicId
view topic =
  Html.div
    [ class "topic cf"
    , Events.onClick topic.id ]
    [ Html.div
      [ class "topic-title" ]
      [ Html.text topic.text ]
    , Html.div
      [ class "opinion-count" ]
      [ Html.span
        [ class "opinion-count-value" ]
        [ Html.text <| toString topic.opinionCount ]
      , Html.i
        [ class "material-icons md-24" ]
        [ Html.text "format_align_left" ]
      ]
    , Html.div
      [ class "topic-created" ]
      [ Html.text <| DateUtils.asString topic.created ]
    , Html.div
      [ class "topic-updated" ]
      [ Html.text <| DateUtils.asString topic.lastUpdated ]
    ]


all : (TopicId -> msg) -> List Topic -> Html msg
all transform topics =
  Html.div [ class "topics" ]
    (List.map (Html.map transform << view ) topics)
