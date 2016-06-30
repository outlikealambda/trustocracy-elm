module Utils.Svg exposing
  ( Dimensions
  , sum
  , rect
  )


import Svg exposing (Svg)
import Svg.Attributes as SvgAttrs



type alias Dimensions =
  { width: Float
  , height: Float
  }


sum : Dimensions -> Dimensions -> Dimensions
sum d1 d2 =
  { width = d1.width + d2.width
  , height = d1.height + d2.height
  }


rect : Float -> Float -> Float -> Float -> String -> Svg msg
rect width height x1 y1 fill =
  Svg.rect
    [ SvgAttrs.x <| toString x1
    , SvgAttrs.y <| toString y1
    , SvgAttrs.width <| toString width
    , SvgAttrs.height <| toString height
    , SvgAttrs.fill fill
    ]
    []
