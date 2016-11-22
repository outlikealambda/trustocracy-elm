module Common.Form exposing
  ( onEnter
  )

import Html
import Html.Events as Events
import Json.Decode as Decode

-- from the Elm Architecture tutorial
onEnter : (Int -> a) -> Html.Attribute a
onEnter transform =
  let
    isEnter code =
      if code == 13 then
        Decode.succeed <| transform code
      else
        Decode.fail "not the right key code"
  in
    Events.on "keydown" <| Decode.andThen isEnter Events.keyCode

