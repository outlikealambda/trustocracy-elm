module Opinion.Writer exposing
  ( Writer
  , Msg
  , update
  , view
  )


import Html exposing (Html)
import Html.App
import Html.Attributes as Attributes exposing (class)
import Html.Events as Events


import Model.Qualifications as Qualifications exposing (Qualifications)
import Update.Qualifications as QualificationsUpdate
import View.Qualifications as QualificationsView


type alias Writer a =
  { a
  | text : String
  , qualifications : Maybe Qualifications
  }


type Msg
  = Write String
  | QualificationsMsg QualificationsUpdate.Msg


update : Msg -> Writer a -> Writer a
update action writer =
  case action of
    Write raw ->
      { writer | text = raw }
    QualificationsMsg msg ->
      { writer
        | qualifications = QualificationsUpdate.update msg writer.qualifications }



view : Writer a -> Html Msg
view {text, qualifications} =
  Html.div
    [ class "writer" ]
    [ Html.App.map Write (viewOpinionInput text)
    , Html.App.map QualificationsMsg (viewQualificationsInput qualifications)
  ]


viewOpinionInput : String -> Html String
viewOpinionInput currentText =
  Html.div [ class "opinion-creator" ]
    [ Html.div [ class "input-field" ]
      [ Html.textarea
        [ class "write"
        , Attributes.placeholder "Let's write something!"
        , Attributes.value currentText
        , Events.on "input" Events.targetValue
        ]
        []
      ]
    -- , div [ class "character-count" ]
    --   [ String.length currentText
    --     |> toString
    --     |> flip (++) " characters written"
    --     |> Html.text
    --   ]
    ]


viewQualificationsInput : Maybe Qualifications -> Html QualificationsUpdate.Msg
viewQualificationsInput qualifications =
  Html.div
    [ class "write-qualifications" ]
    [ Html.div
      [ class "section-header" ]
      [ Html.text "Topic Specific Qualifications" ]
    , Html.div
      [ class "section-sub-header" ]
      [ Html.text "for general qualifications, edit your profile" ]
    , QualificationsView.viewForm qualifications
    ]
