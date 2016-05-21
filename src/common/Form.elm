module Common.Form exposing
  ( onEnter
  )

import Html
import Html.Events as Events
import Json.Decode as Decode

-- from the Elm Architecture tutorial
onEnter : (() -> a) -> Html.Attribute a
onEnter transform =
  let
    decoder = Decode.customDecoder Events.keyCode
      <| Result.map transform << is13
  in
    Events.on "keydown" decoder


is13 : Int -> Result String ()
is13 code =
  if code == 13 then
    Ok ()

  else
    Err "not the right key code"
