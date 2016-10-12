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
import Model.Question.Question exposing (Question)


import Utils.List as ListUtils
import Utils.Date as DateUtils

import View.Author as AuthorView
import View.Opinion as OpinionView
import View.Path as PathView


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
  , readMore : () -> msg
  -- , next : () -> msg
  , questions : List Question
  , isExpanded : Bool
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
      case context.isExpanded of
        True ->
          List.map PathView.view paths

        False ->
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
body context {opinion} =

  case context.isExpanded of
    True ->
      [ OpinionView.text True opinion
      , lastUpdated opinion.created
      , Html.App.map context.showAll showAll
      ]

    False ->
      [ OpinionView.text False opinion
      , Html.App.map context.readMore readMoreButton
      ]


readMoreButton : Html ()
readMoreButton =
  Html.a
    [ class "read-more"
    , Events.onClick ()
    ]
    [ Html.text "read more..."
    ]


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
