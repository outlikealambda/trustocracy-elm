module Opinion
  ( Model
  , init
  , initExpanded
  ) where

import String

import Credentials
import User exposing (User)
import Topic exposing (Topic)

type alias Model =
  { oid: Int
  , expanded: Bool
  , text : String
  , snippet : String
  , credentials : Credentials.Model
  }


init : Int -> Model
init oid =
  Model oid False "" "" Credentials.init


initExpanded : Int -> Model
initExpanded = setExpanded << init


setExpanded : Model -> Model
setExpanded m =
  { m | expanded = True }
