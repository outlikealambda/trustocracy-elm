module View.Qualifications exposing
  ( view
  , viewForm
  , noQualifications
  )


import Model.Qualifications as Qualifications exposing (Qualifications, Qualification)


import Html exposing (Html)
import Html.Attributes as Attributes exposing (class)
import Html.Events as Events


view : Qualifications -> Html msg
view qualifications =
  Html.ul
    [ class "qualifications" ]
    ( List.map viewQualification qualifications )


viewQualification : Qualification -> Html msg
viewQualification {credentials, label} =
  Html.li
    [ class <| "qualification " ++ label ]
    [ Html.text credentials ]


viewForm : Qualifications -> Html Qualification
viewForm qualifications =
  Html.div
    [ class "qualifications-input" ]
    ( List.map viewQualificationForm qualifications
    )


viewQualificationForm : Qualification -> Html Qualification
viewQualificationForm {realm, label, placeholder, credentials} =
  Html.div
    [ class <| "qualification " ++ Qualifications.realmToString realm ]
    [ Html.label
      []
      [ Html.text label ]
    , Html.textarea
      [ class "qualification-input"
      , Attributes.value credentials
      , Attributes.placeholder placeholder
      , Events.onInput (Qualifications.buildQualification realm)]
      []
    ]


noQualifications : Html msg
noQualifications =
  Html.div
    [ class "no-qualifications" ]
    [ Html.text "No qualifications listed" ]
