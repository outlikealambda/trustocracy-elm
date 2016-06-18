module Update.Qualifications exposing
  ( Msg
    (..)
  , update
  )


import Model.Qualifications as Qualifications exposing (Qualifications)


type Msg
  = SetIndustry String
  | SetAcademia String
  | SetPersonal String



update : Msg -> Maybe Qualifications -> Maybe Qualifications
update action maybeQualifications =
  let
    qualifications =
      Maybe.withDefault Qualifications.empty maybeQualifications
    update =
      case action of
        SetIndustry input ->
          { qualifications
          | industry = Qualifications.industryQualification input }
        SetAcademia input ->
          { qualifications
          | academia = Qualifications.academiaQualification input }
        SetPersonal input ->
          { qualifications
          | personal = Qualifications.personalQualification input }
  in
    Qualifications.toMaybe update
