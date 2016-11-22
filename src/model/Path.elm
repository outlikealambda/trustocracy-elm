module Model.Path exposing
  ( Path
  , decoder
  )


import Model.Relationship as Relationship exposing (Relationship)
import Model.Trustee as Trustee exposing (Trustee)


import Json.Decode as Decode


type alias Path =
  { trustee : Trustee
  , hops : List Relationship
  , score : Int
  }


decoder : Decode.Decoder Path
decoder =
  Decode.map3 Path
    (Decode.field "trustee" Trustee.decoder)
    (Decode.field "hops" <| Decode.list Relationship.decoder)
    (Decode.field "score" Decode.int)
