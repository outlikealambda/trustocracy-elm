module Qualifications exposing
  ( Qualifications
  , Action
  , empty
  , update
  , decoder
  , encode
  , hasAnyQualification
  , view
  , viewForm
  )


import Html exposing (Html, ul, li, div, text)
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


type Action
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


update : Action -> Qualifications -> Qualifications
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


view : Qualifications -> Html
view {industry, academia, personal} =
  ul
    [ class "qualifications" ]
    <| viewQualification industry
    ++ viewQualification academia
    ++ viewQualification personal


viewQualification : Qualification -> List Html
viewQualification {value, label} =
  if String.isEmpty value then
    []
  else
    [ li
      [ class <| "qualification " ++ label ]
      [ text value ]
    ]


viewForm : Signal.Address Action -> Qualifications -> Html
viewForm address {industry, academia, personal} =
  div
    [ class "qualifications-input" ]
    [ viewQualificationForm (Signal.forwardTo address SetIndustry) industry
    , viewQualificationForm (Signal.forwardTo address SetAcademia) academia
    , viewQualificationForm (Signal.forwardTo address SetPersonal) personal
    ]


viewQualificationForm : Signal.Address String -> Qualification -> Html
viewQualificationForm address {value, label, placeholder} =
  div
    [ class <| "qualification " ++ label ]
    [ Html.label
      []
      [ text label ]
    , Html.textarea
      [ class "qualification-input"
      , Attributes.value value
      , Attributes.placeholder placeholder
      , Events.on "input" Events.targetValue (Signal.message address) ]
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
