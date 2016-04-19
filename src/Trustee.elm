module Trustee
  ( Trustee
  , empty
  , decoder
  , encoder
  , fromSelf
  , isRelated
  , setRelationship
  ) where


import Json.Decode as Decode exposing ((:=))
import Json.Encode as Encode
import Common.Relationship as Relationship exposing (Relationship)


type alias Trustee =
  { name: String
  , id: Int
  , relationship: Relationship
  }


empty : Trustee
empty =
  { name = ""
  , id = -1
  , relationship = Relationship.Distant
  }


decoder : Decode.Decoder Trustee
decoder =
  Decode.object3 Trustee
    ( "name" := Decode.string )
    ( "id" := Decode.int )
    ( Decode.oneOf
      [ "relationship" := Relationship.decoder
      , Decode.succeed Relationship.Distant
      ]
    )


encoder : Trustee -> Encode.Value
encoder {name, id, relationship} =
  Encode.object
    [ ( "name", Encode.string name )
    , ( "id", Encode.int id )
    , ( "relationship", Relationship.encoder relationship )
    ]


fromSelf : String -> Int -> Trustee
fromSelf name id =
  { name = name
  , id = id
  , relationship = Relationship.Self
  }


isRelated : Relationship -> Trustee -> Bool
isRelated relationship trustee =
  relationship == trustee.relationship


setRelationship : Relationship -> Trustee -> Trustee
setRelationship relationship trustee =
  { trustee | relationship = relationship }
