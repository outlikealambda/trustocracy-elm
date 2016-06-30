module View.Path exposing
  ( view )


import Model.Path as Path exposing (Path)
import Model.Relationship as Relationship
import Utils.Svg as SvgUtils


import Html exposing (Html)
import Html.Attributes exposing (class)
import String
import Svg exposing (Svg)
import Svg.Attributes as SvgAttrs


view : Path -> Html msg
view {trustee, hops} =
  Html.div
    [ class "path cf" ]
    [ Html.div
      [ class <| "trustee " ++ Relationship.toClass trustee.relationship ]
      [ Html.text trustee.name ]
    , Html.div
      [ class "hops" ]
      [ List.map Relationship.toSvg hops
        |> List.intersperse (arrow 15 30)
        |> buildPath
      ]
    ]

type alias Settings =
  { spacing : Float
  , height : Float
  }


settings : Settings
settings =
  Settings
    20
    40


arrow : Float -> Float -> Float -> Float -> (Svg msg, SvgUtils.Dimensions)
arrow width height x1 lineHeight =
  let
    point x y =
      (toString x) ++ "," ++ (toString y)
    topY =
      (lineHeight - height) / 2
    topX =
      x1
    topPoint =
      point topX topY
    middleY =
      lineHeight / 2
    middleX =
      x1 + width
    middlePoint =
      point middleX middleY
    bottomY =
      topY + height
    bottomX =
      x1
    bottomPoint =
      point bottomX bottomY
    points =
      String.join
        " "
        [ bottomPoint
        , middlePoint
        , topPoint
        ]
  in
    ( Svg.polyline
      [ SvgAttrs.fill "none"
      , SvgAttrs.stroke "#111888"
      , SvgAttrs.strokeWidth "3"
      , SvgAttrs.strokeLinecap "round"
      , SvgAttrs.points points
      ]
      []
    , SvgUtils.Dimensions width height
    )


buildPath : List (Float -> Float -> (Svg msg, SvgUtils.Dimensions)) -> Html msg
buildPath builders =
  let
    {svgs, totalWidth} =
      List.foldl (reduce settings.height) (Accumulator [] 0) builders
  in
    Svg.svg
      [ SvgAttrs.width <| toString totalWidth
      , SvgAttrs.height <| toString settings.height
      ]
      svgs


type alias Accumulator msg =
  { svgs : List (Svg msg)
  , totalWidth: Float
  }


reduce : Float -> (Float -> Float -> (Svg msg, SvgUtils.Dimensions)) -> Accumulator msg -> Accumulator msg
reduce y1 svgBuilder {svgs, totalWidth} =
  let
    x1 =
      totalWidth + settings.spacing
    (svg, svgDimensions) =
      svgBuilder x1 y1

  in
    Accumulator
      (List.append svgs [svg])
      (x1 + svgDimensions.width)
