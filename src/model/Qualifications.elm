module Model.Qualifications exposing
  ( Qualifications
  , Qualification
  , empty
  , decoder
  , encode
  , buildQualification
  , realmToString
  , setQualification
  , removeEmpty
  )


import Json.Decode as Decode
import Json.Encode as Encode
import String


type alias Qualifications = List Qualification


type Realm
  = Industry
  | Academia
  | Personal
  | Unknown


type alias Qualification =
  { realm : Realm
  , label : String
  , placeholder : String
  , credentials : String
  }


placeholder : {industry:String, academia:String, personal:String, unknown:String}
placeholder =
  { industry = "I was a professional watermelon carver for 5 years"
  , academia = "I wrote my thesis on watermelon juggling"
  , personal = "I once ate 3 entire watermelons in a single sitting"
  , unknown = "I did things! (but not sure what)"
  }


toRealm : String -> Realm
toRealm raw =
  case String.toLower raw of
    "industry" ->
      Industry
    "academia" ->
      Academia
    "personal" ->
      Personal
    _ ->
      Unknown


realmToString : Realm -> String
realmToString realm =
  case realm of
    Industry ->
      "industry"
    Academia ->
      "academia"
    Personal ->
      "personal"
    Unknown ->
      "unknown"


buildQualification : Realm -> String -> Qualification
buildQualification realm =
  Qualification
    realm
    (realmToString realm)
    (getPlaceholder realm)


getPlaceholder : Realm -> String
getPlaceholder realm =
  case realm of
    Industry ->
      placeholder.industry
    Academia ->
      placeholder.academia
    Personal ->
      placeholder.personal
    Unknown ->
      placeholder.unknown


empty : Qualifications
empty =
  [ buildQualification Industry ""
  , buildQualification Academia ""
  , buildQualification Personal ""
  ]


setQualification : Qualification -> Qualifications -> Qualifications
setQualification q qs =
  let
    replaceIfMatch =
      \qa qb ->
        if qa.realm /= qb.realm then
          qa
        else
          qb
    append =
      \item list ->
        (replaceIfMatch item q) :: list
  in
    List.foldr append [] qs
    |> removeEmpty


removeEmpty : Qualifications -> Qualifications
removeEmpty =
  List.filter (not << String.isEmpty << .credentials)


fromApi : List (String, String) -> Qualifications
fromApi raws =
  let
    createQualification =
      \(rawRealm, creds) -> buildQualification (toRealm rawRealm) creds
  in
    List.map createQualification raws


decoder : Decode.Decoder Qualifications
decoder =
  Decode.map
    fromApi
    (Decode.keyValuePairs Decode.string)


encode : Qualifications -> Encode.Value
encode qualifications =
  List.map
    (\{realm, credentials} -> (realmToString realm, Encode.string credentials))
    (removeEmpty qualifications)
    |> Encode.object
