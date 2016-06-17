module View.Connection exposing
  ( view
  , Context
  )


import Model.Connection as Connection exposing (Connection)
import Model.Expandable as Expandable exposing (Expandable)
import Utils.List as ListUtils


import View.Author as AuthorView
import View.Opinion as OpinionView
import View.Path as PathView


import Html exposing (Html)
import Html.App
import Html.Attributes exposing (class)
import Html.Events as Events


type alias OpinionId = Int


type alias Context msg =
  { showAll : (() -> msg)
  , readMore : (OpinionId -> msg)
  }


view : Context msg -> Connection -> Html msg
view context {author, opinion, paths, status} =
  case status of

    Expandable.Expanded ->
      Html.div
        [ class "connection" ]
        [ Html.div
          [ class "paths" ]
          ( List.map PathView.view paths )
        , AuthorView.view author opinion.influence opinion.qualifications
        , OpinionView.view True opinion
        , Html.App.map context.showAll showAll
        ]

    Expandable.Collapsed ->
      Html.div
        [ class "connection" ]
        [ Html.div
          [ class "paths" ]
          ( List.head (Debug.log "all paths" paths)
            |> Maybe.map PathView.view
            |> Maybe.map ListUtils.singleton
            |> Maybe.withDefault []
          )
        , AuthorView.view author opinion.influence opinion.qualifications
        , OpinionView.view False opinion
        -- insert read more here
        , Html.App.map context.readMore <| readMore opinion.id
        ]


readMore : OpinionId -> Html OpinionId
readMore oid =
  Html.a
    [ class "read-more"
    , Events.onClick oid
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
