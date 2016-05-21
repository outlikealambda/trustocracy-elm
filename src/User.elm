module User exposing
  ( User
  , decoder
  , encode
  , empty
  , isEmpty
  )

import Json.Decode as Decode exposing ((:=))
import Json.Encode as Encode
import Trustee exposing (Trustee)


type alias User =
  { name: String
  , id: Int
  , trustees: List Trustee
  , emails: List Email
  }


type alias Email = String


empty : User
empty =
  { name = ""
  , id = -1
  , trustees = []
  , emails = []
  }

decoder : Decode.Decoder User
decoder =
  Decode.object4 User
    ("name" := Decode.string)
    ("id" := Decode.int)
    ("trustees" := Decode.list Trustee.decoder)
    ("emails" := Decode.list Decode.string)


encode : User -> Encode.Value
encode {name, id, trustees} =
  Encode.object
    [ ("name", Encode.string name)
    , ("id", Encode.int id)
    , ("trustees", Encode.list <| List.map Trustee.encoder trustees)
    ]


isEmpty : User -> Bool
isEmpty user =
  user.id == -1
