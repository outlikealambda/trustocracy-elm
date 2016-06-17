module Opinion.Writer exposing
  ( Writer
  , Msg
  , update
  , view
  )


import Html exposing (Html, div, textarea)
import Html.App
import Html.Attributes exposing (class, placeholder, value)
import Html.Events exposing (on, targetValue)


import Model.Qualifications as Qualifications exposing (Qualifications)
import View.Qualifications as QualificationsView


type alias Writer a =
  { a
    | text : String
    , qualifications : Qualifications
  }


type Msg
  = Write String
  | QualificationsMsg Qualifications.Msg


update : Msg -> Writer a -> Writer a
update action writer =
  case action of
    Write raw ->
      { writer | text = raw }
    QualificationsMsg msg ->
      { writer
        | qualifications = Qualifications.update msg writer.qualifications }



view : Writer a -> Html Msg
view {text, qualifications} =
  div
    [ class "writer" ]
    [ Html.App.map Write (viewOpinionInput text)
    , Html.App.map QualificationsMsg (viewQualificationsInput qualifications)
    ]


viewOpinionInput : String -> Html String
viewOpinionInput currentText =
  div [ class "opinion-creator" ]
    [ div [ class "input-field" ]
      [ textarea
        [ class "write"
        , placeholder "Let's write something!"
        , value currentText
        , on "input" targetValue
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


viewQualificationsInput : Qualifications -> Html Qualifications.Msg
viewQualificationsInput qualifications =
  div
    [ class "write-qualifications" ]
    [ div
      [ class "section-header" ]
      [ Html.text "Topic Specific Qualifications" ]
    , div
      [ class "section-sub-header" ]
      [ Html.text "for general qualifications, edit your profile" ]
    , QualificationsView.viewForm qualifications
    ]
