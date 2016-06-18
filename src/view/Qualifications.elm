module View.Qualifications exposing
  ( view
  , viewForm
  , noQualifications
  )


import Model.Qualifications as Qualifications exposing (Qualifications, Qualification)
import Update.Qualifications as QualificationsUpdate exposing (Msg)


import Html exposing (Html)
import Html.App
import Html.Attributes as Attributes exposing (class)
import Html.Events as Events
import String


view : Qualifications -> Html msg
view {industry, academia, personal} =
  Html.ul
    [ class "qualifications" ]
    <| viewQualification industry
    ++ viewQualification academia
    ++ viewQualification personal


viewQualification : Qualification -> List (Html msg)
viewQualification {value, label} =
  if String.isEmpty value then
    []
  else
    [ Html.li
      [ class <| "qualification " ++ label ]
      [ Html.text value ]
    ]


viewForm : Maybe Qualifications -> Html Msg
viewForm maybeQualifications =
  let
    {industry, academia, personal} =
      Maybe.withDefault Qualifications.empty maybeQualifications
  in
    Html.div
      [ class "qualifications-input" ]
      [ Html.App.map QualificationsUpdate.SetIndustry (viewQualificationForm industry)
      , Html.App.map QualificationsUpdate.SetAcademia (viewQualificationForm academia)
      , Html.App.map QualificationsUpdate.SetPersonal (viewQualificationForm personal)
      ]


viewQualificationForm : Qualification -> Html String
viewQualificationForm {value, label, placeholder} =
  Html.div
    [ class <| "qualification " ++ label ]
    [ Html.label
      []
      [ Html.text label ]
    , Html.textarea
      [ class "qualification-input"
      , Attributes.value value
      , Attributes.placeholder placeholder
      , Events.on "input" Events.targetValue ]
      []
    ]

noQualifications : Html msg
noQualifications =
  Html.div
    [ class "no-qualifications" ]
    [ Html.text "No qualifications listed" ]
