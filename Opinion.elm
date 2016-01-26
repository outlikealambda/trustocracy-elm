module Opinion
  ( Model
  , init
  ) where

import String

import Credentials
import User exposing (User)
import Topic exposing (Topic)

type alias Model =
  { id: Int
  , expanded: Bool
  , text : String
  , snippet : String
  , tid : Topic
  , user: User
  , credentials : Credentials.Model
  }

init : Int -> User -> Topic -> Model
init oid user topic =
  Model oid False "" "" topic user Credentials.init
