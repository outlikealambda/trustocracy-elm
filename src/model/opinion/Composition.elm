module Model.Opinion.Composition exposing
  ( Composition
  , empty
  , decoder
  , encode
  , key
  )


import Model.Extend.Createable as Createable exposing (Createable)
import Model.Opinion.Record as Record exposing (Record)
import Model.Qualifications as Qualifications exposing (Qualifications)
import Model.Trustee as Trustee exposing (Trustee)


import Utils.String as StringUtils


import Date exposing (Date)
import Json.Decode as Decode exposing ((:=))
import Json.Encode as Encode
import Time


type alias Composition = Createable (Record {})


key : Composition -> Maybe Int
key = .id


empty : Composition
empty =
  { id = Nothing
  , text = ""
  , influence = -1
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
    , ("influence", Encode.int composition.influence)
    ]


decoder : Decode.Decoder Composition
decoder =
  Decode.object6 fromApi
    ( Decode.oneOf
      [ "id" := Decode.map Just Decode.int
      , Decode.succeed Nothing
      ]
    )
    ("text" := Decode.string)
    ("influence" := Decode.int)
    ("author" := Trustee.decoder)
    (Decode.oneOf
      [ "qualifications" := Qualifications.decoder
      , Decode.succeed Qualifications.empty
      ]
    )
    ("created" := Decode.float)


fromApi : Maybe Int -> String -> Int -> Trustee -> Qualifications -> Float -> Composition
fromApi id text influence author qualifications created =
  { id = id
  , text = text
  , influence = influence
  , author = author
  , qualifications = qualifications
  , created = Date.fromTime <| Time.second * created
  , fetched = True
  , snippet = StringUtils.snippetize 100 text
  , preview = StringUtils.snippetize 300 text
  , expanded = False
  }
