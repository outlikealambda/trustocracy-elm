module View.Question.Metrics exposing
  ( view
  )


import Model.Connection.Metrics exposing (Metrics)
import Model.Question.Question as Question exposing (Question)


import Dict
import Html exposing (Html)
import Html.Attributes exposing (class)
import Svg
import Svg.Attributes as SvgAttr


type alias Context =
  { tColor : String
  , bColor : String
  }


view : List Question -> Metrics -> Html msg
view questions metrics =
  let
    coloredBuilder =
      meter { tColor = "#42c5f4" , bColor = "#f4a442" }

    raterLabels =
      Question.raters questions

    buildTri (qid, rating) =
      let
        ratedBuilder =
          coloredBuilder rating
      in
        Dict.get qid raterLabels
          |> Maybe.map (\(tOption, bOption) -> (tOption.label, bOption.label))
          |> Maybe.map ratedBuilder

  in
    Html.div
      [ class "metrics" ]
      (List.filterMap buildTri metrics)


meter : Context -> Float -> (String, String) -> Html msg
meter {tColor, bColor} vPosFloat (tLabel, bLabel) =
  let
    w =
      20

    h =
      50

    vPos =
      round <| (toFloat (h - 10) / 100) * vPosFloat + 5

    xleft =
      2

    xright =
      w - 2

    triBuilder =
      flip (triangleD xleft xright) <| vPos

    midLine =
      "M" ++ toString (xleft - 2)
        ++ " "
        ++ toString vPos
        ++ " L"
        ++ toString (xright + 2)
        ++ " "
        ++ toString vPos
  in
    Html.div
      [ class "meter" ]
      [ Html.div [ class "top-label" ] [ Html.text tLabel ]
      , Svg.svg
        [ SvgAttr.width <| toString w
        , SvgAttr.height <| toString h
        , SvgAttr.class "tri-meter"
        ]
        [ Svg.path
          [ SvgAttr.d <| triBuilder <| 5
          , SvgAttr.strokeWidth "2"
          , SvgAttr.stroke tColor
          , SvgAttr.fill tColor
          , SvgAttr.strokeLinejoin "round"
          , SvgAttr.class "up-tri tri"
          ]
          []
        , Svg.path
          [ SvgAttr.d <| triBuilder <| h - 5
          , SvgAttr.strokeWidth "2"
          , SvgAttr.stroke bColor
          , SvgAttr.fill bColor
          , SvgAttr.strokeLinejoin "round"
          , SvgAttr.class "down-tri tri"
          ]
          []
        , Svg.path
          [ SvgAttr.d midLine
          , SvgAttr.strokeWidth "2"
          , SvgAttr.stroke "#292292"
          , SvgAttr.strokeLinecap "round"
          ]
          []
        ]
      , Html.div [ class "bottom-label" ] [ Html.text bLabel ]
      ]

triangleD : Int -> Int -> Int -> Int -> String
triangleD x1base x2base ytip ybase =
  let
    midpt =
      round <| (toFloat x1base + toFloat x2base) / 2
  in
    "M" ++ toString midpt
      ++ " " ++ toString ytip -- tip
      ++ " L" ++ toString x1base ++ " " ++ toString ybase -- base-left
      ++ " L" ++ toString x2base ++ " " ++ toString ybase -- base-right
      ++ " Z" -- tip
