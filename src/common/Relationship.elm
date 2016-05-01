module Common.Relationship
  ( Relationship
    ( Bff
    , Trusted
    , Public
    , Candidate
    , Self
    , None
    )
  , view
  , decoder
  , encoder
  , toString
  , toReadable
  ) where


import Html exposing (Html, div, span)
import Html.Attributes exposing (class)
import Json.Decode as Decode
import Json.Encode as Encode


type Relationship
  = Bff
  | Trusted
  | Public
  | Candidate
  | None
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
    "KNOWS" ->
      Candidate
    _ ->
      None


toString : Relationship -> String
toString relationship =
  case relationship of
    Bff ->
      "TRUSTS_EXPLICITLY"
    Trusted ->
      "TRUSTS"
    Public ->
      "PUBLIC"
    Candidate ->
      "KNOWS"
    Self ->
      "SELF"
    None ->
      "NONE"

toReadable : Relationship -> String
toReadable r =
  case r of
    Bff ->
      "Bff"
    Trusted ->
      "Trusted"
    Public ->
      "Public"
    Candidate ->
      "Candidate"
    Self ->
      "Self"
    None ->
      "None"
