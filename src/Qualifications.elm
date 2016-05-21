module Qualifications exposing
  ( Qualifications
  , Msg
  , empty
  , update
  , decoder
  , encode
  , hasAnyQualification
  , view
  , viewForm
  )


import Html exposing (Html, ul, li, div, text)
import Html.App
import Html.Attributes as Attributes exposing (class)
import Html.Events as Events
import Json.Decode as Decode exposing ((:=))
import Json.Encode as Encode
import String


type alias Qualifications =
  { industry : Qualification
  , academia : Qualification
  , personal : Qualification
  }


type alias Qualification =
  { value : String
  , label : String
  , placeholder : String
  }


type Msg
  = SetIndustry String
  | SetAcademia String
  | SetPersonal String


empty : Qualifications
empty = init "" "" ""


init : String -> String -> String -> Qualifications
init industry academia personal =
  { industry = industryQualification industry
  , academia = academiaQualification academia
  , personal = personalQualification personal
  }


update : Msg -> Qualifications -> Qualifications
update action qualifications =
  case action of
    SetIndustry input ->
      { qualifications | industry = industryQualification input }
    SetAcademia input ->
      { qualifications | academia = academiaQualification input }
    SetPersonal input ->
      { qualifications | personal = personalQualification input }


decoder : Decode.Decoder Qualifications
decoder =
  Decode.object3 init
    ( "industry" := Decode.string )
    ( "academia" := Decode.string )
    ( "personal" := Decode.string )


encode : Qualifications -> Encode.Value
encode {industry, academia, personal} =
  Encode.object
    [ ("industry", Encode.string industry.value)
    , ("academia", Encode.string academia.value)
    , ("personal", Encode.string personal.value)
    ]


hasAnyQualification : Qualifications -> Bool
hasAnyQualification qualifications =
  let fields =
    [ .industry qualifications
    , .academia qualifications
    , .personal qualifications
    ]
  in
    List.any (not << String.isEmpty << .value) fields


view : Qualifications -> Html msg
view {industry, academia, personal} =
  ul
    [ class "qualifications" ]
    <| viewQualification industry
    ++ viewQualification academia
    ++ viewQualification personal


viewQualification : Qualification -> List (Html msg)
viewQualification {value, label} =
  if String.isEmpty value then
    []
  else
    [ li
      [ class <| "qualification " ++ label ]
      [ text value ]
    ]


viewForm : Qualifications -> Html Msg
viewForm {industry, academia, personal} =
  div
    [ class "qualifications-input" ]
    [ Html.App.map SetIndustry (viewQualificationForm industry)
    , Html.App.map SetAcademia (viewQualificationForm academia)
    , Html.App.map SetPersonal (viewQualificationForm personal)
    ]


viewQualificationForm : Qualification -> Html String
viewQualificationForm {value, label, placeholder} =
  div
    [ class <| "qualification " ++ label ]
    [ Html.label
      []
      [ text label ]
    , Html.textarea
      [ class "qualification-input"
      , Attributes.value value
      , Attributes.placeholder placeholder
      , Events.on "input" Events.targetValue ]
      []
    ]


-- private
industryQualification : String -> Qualification
industryQualification value =
  Qualification value "industry" "I was a professional watermelon carver for 5 years"


academiaQualification : String -> Qualification
academiaQualification value =
  Qualification value "academia" "I wrote my thesis on watermelon juggling"


personalQualification : String -> Qualification
personalQualification value =
  Qualification value "personal" "I once ate 3 entire watermelons in a single sitting"
