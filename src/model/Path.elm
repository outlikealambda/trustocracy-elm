module Model.Path exposing
  ( Path
  , decoder
  )


import Model.Relationship as Relationship exposing (Relationship)
import Model.Trustee as Trustee exposing (Trustee)


import Json.Decode as Json exposing ((:=))


type alias Path =
  { trustee : Trustee
  , hops : List Relationship
  , score : Int
  }


decoder : Json.Decoder Path
decoder =
  Json.object3 Path
    ("trustee" := Trustee.decoder)
    ("hops" := Json.list Relationship.decoder)
    ("score" := Json.int)
