module Common.Form exposing
  ( onEnter
  )

import Html
import Html.Events as Events
import Json.Decode as Decode

-- from the Elm Architecture tutorial
onEnter : Signal.Address a -> a -> Html.Attribute
onEnter address value =
  Events.on "keydown"
    (Decode.customDecoder Events.keyCode is13)
    (\_ -> Signal.message address value)


is13 : Int -> Result String ()
is13 code =
  if code == 13 then
    Ok ()

  else
    Err "not the right key code"
