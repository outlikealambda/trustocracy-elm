module Common.Relationship
  ( Relationship
    ( Bff
    , Trusted
    , Distant
    , Self )
  , view
  , decoder
  , encoder
  ) where


import Html exposing (Html, div, span)
import Html.Attributes exposing (class)
import Json.Decode as Decode
import Json.Encode as Encode


type Relationship
  = Bff
  | Trusted
  | Public
  | Distant
  | Self


view : Relationship -> Html
view relationship =
  span [class (getCircleType relationship)] []


getCircleType : Relationship -> String
getCircleType relationship =
  case relationship of
    Bff -> "circle-explicit"
    Trusted -> "circle-regular"
    _ -> "circle-unknown"


decoder : Decode.Decoder Relationship
decoder =
  Decode.map toRelationship Decode.string


encoder : Relationship -> Encode.Value
encoder =
  Encode.string << toString


toRelationship : String -> Relationship
toRelationship asString =
  case asString of
    "TRUSTS_EXPLICITLY" ->
      Bff
    "TRUSTS" ->
      Trusted
    _ ->
      Distant


toString : Relationship -> String
toString relationship =
  case relationship of
    Bff ->
      "TRUSTS_EXPLICITLY"
    Trusted ->
      "TRUSTS"
    Public ->
      "PUBLIC"
    Distant ->
      "DISTANT"
    Self ->
      "SELF"
