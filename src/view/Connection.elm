module View.Connection exposing
  ( view
  , Context
  )


import Common.Extended as Extended
import Common.Remote exposing
  ( Remote
    ( NoRequest
    , Requested
    , Retrieved
    )
  )

import Model.Connection.Connection as Connection exposing (Connection)
import Model.Connection.Details exposing (Details)
import Model.Connection.Link exposing (Link)
import Model.Extend.Expandable as Expandable exposing (Expandable)
import Model.Question.Question exposing (Question)

import Update.Connection as Update

import Utils.List as ListUtils
import Utils.Date as DateUtils

import View.Author as AuthorView
import View.Opinion as OpinionView
import View.Path as PathView
import View.Question.Assessor as AssessorView


import Html exposing (Html)
import Html.App
import Html.Attributes as Attrs exposing (class)
import Html.Events as Events
import Date exposing (Date)


type alias OpinionId = Int
type alias Qid = Int
type alias Key = Int


type alias Context msg =
  { showAll : () -> msg
  , readMore : Update.Msg -> msg
  , next : Update.Msg -> msg
  , questions : List Question
  }


view : Context msg -> Connection -> Html msg
view context connection =
  case connection of
    Extended.Basic details ->
      public context details

    Extended.Complex details link ->
      connected context details link


public : Context msg -> Details -> Html msg
public context details =
  Html.div
    [ class "disjoint cf" ]
    <|
      [ Html.div
        [ class "connection-header cf" ]
        [ AuthorView.connection <| .author <| .opinion details
        , viewInfluence details.influence
        ]
      ]
      ++
        body context details


connected : Context msg -> Details -> Link -> Html msg
connected context details link =
  let

    buildPathElements paths =
      case details.inflation of
        Expandable.Expanded ->
          List.map PathView.view paths

        Expandable.Collapsed ->
          List.head paths
            |> Maybe.map PathView.view
            |> Maybe.map ListUtils.singleton
            |> Maybe.withDefault []

  in
    Html.div
      [ class "connection cf" ]
      <|
        [ Html.div
          [ class "connection-header cf" ]
          [ Html.div
            [ class "paths" ]
            (buildPathElements link.userLink)
          , AuthorView.connection <| .author <| .opinion details
          , viewInfluence details.influence
          ]
        ]
        ++
          body context details


body : Context msg -> Details -> List (Html msg)
body context {opinion, influence, assessor, inflation} =
  let
    opinionView isExpanded =
      OpinionView.text isExpanded opinion

    influenceView =
      viewInfluence influence

    assessorView =
      AssessorView.questions context.questions assessor
        |> Html.App.map Update.DelegateToAssessor
        |> Html.App.map context.next

  in
    case inflation of
      Expandable.Expanded ->
        [ opinionView True
        , lastUpdated opinion.created
        , assessorView
        , Html.App.map context.showAll showAll
        ]

      Expandable.Collapsed ->
        [ opinionView False
        , Html.App.map context.readMore readMoreButton
        ]


readMoreButton : Html Update.Msg
readMoreButton =
  Html.a
    [ class "read-more"
    , Events.onClick ()
    ]
    [ Html.text "read more..."
    ]
    |> Html.App.map (always Update.Zoom)


showAll : Html ()
showAll =
  Html.a
    [ class "show-all"
    , Events.onClick ()
    ]
    [ Html.text "show all..."
    ]


lastUpdated : Date -> Html msg
lastUpdated date =
  Html.div
    [ class "last-updated" ]
    [ Html.text <| DateUtils.asString date ]


viewInfluence : Remote Int -> Html msg
viewInfluence remoteInfl =
  case remoteInfl of
    NoRequest ->
      Html.div
        [ class "influence no-request" ]
        [ Html.text "Couldn't calculate influence :("]

    Requested ->
      Html.div
        [ class "influence requested" ]
        [ Html.text "Calculating Influence..." ]

    Retrieved influence ->
      Html.div
        [ class "influence retrieved" ]
        [ Html.text <| toString influence ++ " connected users" ]
