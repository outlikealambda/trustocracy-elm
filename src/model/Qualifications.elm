module Model.Qualifications exposing
  ( Qualifications
  , Qualification
  , empty
  , decoder
  , encode
  , toMaybe
  , hasAnyQualification
  , industryQualification
  , academiaQualification
  , personalQualification
  )


import Json.Decode as Decode exposing ((:=))
import Json.Encode as Encode
import String


type alias Qualifications =
  { industry : Qualification
  , academia : Qualification
  , personal : Qualification
  }


type alias Qualification =
  { label : String
  , placeholder : String
  , value : String
  }


empty : Qualifications
empty = fromApi "" "" ""


fromApi : String -> String -> String -> Qualifications
fromApi industry academia personal =
  { industry = industryQualification industry
  , academia = academiaQualification academia
  , personal = personalQualification personal
  }


toMaybe : Qualifications -> Maybe Qualifications
toMaybe qualifications =
  if hasAnyQualification qualifications then
    Just qualifications
  else
    Nothing


decoder : Decode.Decoder Qualifications
decoder =
  Decode.object3 fromApi
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


-- private
industryQualification : String -> Qualification
industryQualification =
  Qualification "industry" "I was a professional watermelon carver for 5 years"


academiaQualification : String -> Qualification
academiaQualification =
  Qualification "academia" "I wrote my thesis on watermelon juggling"


personalQualification : String -> Qualification
personalQualification =
  Qualification "personal" "I once ate 3 entire watermelons in a single sitting"
