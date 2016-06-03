module Topic.Delegation exposing
  ( Delegation
  , decoder
  )

import Json.Decode as Json exposing ((:=))

import User exposing (User)
import Trustee exposing (Trustee)
import Topic.Model as Topic exposing (Topic)


type alias Delegation =
  { from : User
  , for : Topic
  , to : Trustee
  }


decoder : Json.Decoder Delegation
decoder =
  Json.object3
    (\from for to -> Delegation from for to)
    ("from" := User.decoder)
    ("for" := Topic.decoder)
    ("to" := Trustee.decoder)
