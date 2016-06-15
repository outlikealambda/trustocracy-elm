module Model.Connection exposing
  ( Connection
  , decoder
  )


import Model.Expandable as Expandable exposing (Expandable)
import Model.Opinion as Opinion exposing (Opinion)
import Model.Path as Path exposing (Path)
import Model.Trustee as Trustee exposing (Trustee)


import Json.Decode as Decode exposing ((:=))


type alias Connection =
  Expandable
    { opinion : Opinion
    , author : Trustee
    , paths : List Path
    }


decoder : Decode.Decoder Connection
decoder =
  Decode.object3 fromApi
    ("opinion" := Opinion.decoder)
    ("author" := Trustee.decoder)
    ("paths" := Decode.list Path.decoder)


fromApi : Opinion -> Trustee -> List Path -> Connection
fromApi opinion author paths =
  { opinion = opinion
  , author = author
  , paths = paths
  , status = Expandable.Collapsed
  }
