module View.Explorer exposing
  ( exploreButton
  , connectedButton
  , view
  )


import Model.SurfacedOpinion.SurfacedOpinion as SurfacedOpinion
import Model.Explorer as Explorer exposing (Explorer)
import Model.Question.Question exposing (Question)

import Update.Explorer as Update

import View.SurfacedOpinion as SurfacedOpinionView
import View.Question.Assessor as AssessorView


import Dict
import Html exposing (Html)
import Html.Attributes exposing (class)
import Html.App
import Html.Events as Events
import String


view : Explorer -> Html Update.Msg
view explorer =
  case explorer.zoom of
    Explorer.Blurred ->
      blurred explorer

    Explorer.Focused oid ->
      focused oid explorer


focused : Int -> Explorer -> Html Update.Msg
focused oid {surfacedOpinions, questions, assessor} =
  let
    context =
      buildContext True questions oid

    assessorView =
      AssessorView.questions questions assessor
      |> Html.App.map (Update.DelegateToAssessor oid)

    combineViews surfacedOpinionView =
      Html.div
        [ class "explorer focused" ]
        [ surfacedOpinionView
        , assessorView
        ]
  in
    Dict.get oid surfacedOpinions
    |> Maybe.map (SurfacedOpinionView.view context)
    |> Maybe.map combineViews
    |> Maybe.withDefault (Html.div [] [])


blurred : Explorer -> Html Update.Msg
blurred explorer =
  let
    contextBuilder =
      buildContext False explorer.questions

    viewSurfacedOpinion surfacedOpinion =
      let
        context =
          contextBuilder <| SurfacedOpinion.key surfacedOpinion
      in
        SurfacedOpinionView.view context surfacedOpinion

  in
    Html.div
      [ class "explorer blurred" ]

      ( Explorer.sortSurfacedOpinions explorer
        |> List.map viewSurfacedOpinion
        |> List.intersperse (Html.hr [] [])
        |> (::) (sortButton explorer)
      )


sortButton : Explorer -> Html Update.Msg
sortButton {sort} =
  Html.div
    [ class <| "sort-button " ++ Explorer.classifySort sort
    , Events.onClick Update.NextSort ]
    [ Html.text "By Influence"]


connectedButton : Explorer -> Html msg
connectedButton {surfacedOpinions} =
  String.join " "
    [ toString <| SurfacedOpinion.countLinked <| Dict.values surfacedOpinions
    , "Linked Opinions"
    ]
    |> navButton


exploreButton : Explorer -> Html msg
exploreButton ignored =
  "Explore"
    |> navButton


navButton : String -> Html msg
navButton label =
  Html.div
    [ class "fetched" ]
    [ Html.text label ]


buildContext : Bool -> List Question -> Int -> SurfacedOpinionView.Context Update.Msg
buildContext isExpanded questions surfacedOpinionKey =
  { showAll = Update.Blur
  , readMore = \_ -> Update.Focus surfacedOpinionKey
  , questions = questions
  , isExpanded = isExpanded
  }
