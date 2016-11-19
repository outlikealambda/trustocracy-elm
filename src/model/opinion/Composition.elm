module Model.Opinion.Composition exposing
  ( Composition
  , empty
  , decoder
  , encode
  , key
  )


import Model.Extend.Createable as Createable exposing (Createable)
import Model.Opinion.Record as Record exposing (Record)
import Model.Trustee as Trustee exposing (Trustee)

import Date exposing (Date)
import Json.Decode as Decode exposing ((:=))
import Json.Encode as Encode


type alias Composition = Createable (Record {})


key : Composition -> Maybe Int
key = .id


empty : Composition
empty =
  { id = Nothing
  , text = ""
  , author = Trustee.empty
  , qualifications = []
  , created = Date.fromTime 0
  , snippet = ""
  , preview = ""
  , expanded = False
  , fetched = False
  }


encode : Composition -> Encode.Value
encode composition =
  Encode.object
    [ ("id", Maybe.map Encode.int (key composition) |> Maybe.withDefault Encode.null)
    , ("text", Encode.string composition.text)
    ]


decoder : Decode.Decoder Composition
decoder =
  Decode.object2 fromApi
    ( Decode.oneOf
      [ "id" := Decode.map Just Decode.int
      , Decode.succeed Nothing
      ]
    )
    Record.decoder


fromApi : Maybe Int -> Record a -> Composition
fromApi id record =
  { id = id
  , text = record.text
  , author = record.author
  , qualifications = record.qualifications
  , created = record.created
  , fetched = record.fetched
  , snippet = record.snippet
  , preview = record.preview
  , expanded = record.expanded
  }
