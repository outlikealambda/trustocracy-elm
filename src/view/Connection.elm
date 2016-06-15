module View.Connection exposing (..)


import Model.Connection as Connection exposing (Connection)
import Model.Expandable as Expandable exposing (Expandable)
import Utils.List as ListUtils


import View.Author as AuthorView
import View.Opinion as OpinionView
import View.Path as PathView


import Html exposing (Html)
import Html.Attributes exposing (class)


view : Connection -> Html msg
view {author, opinion, paths, status} =
  case status of

    Expandable.Expanded ->
      Html.div
        [ class "connection" ]
        [ Html.div
          [ class "paths" ]
          ( List.map PathView.view paths )
        , AuthorView.view author opinion.influence opinion.qualifications
        , OpinionView.view True opinion
        ]

    Expandable.Collapsed ->
      Html.div
        [ class "connection" ]
        [ Html.div
          [ class "paths" ]
          ( List.head paths
            |> Maybe.map PathView.view
            |> Maybe.map ListUtils.singleton
            |> Maybe.withDefault []
          )
        , AuthorView.view author opinion.influence opinion.qualifications
        , OpinionView.view False opinion
        -- insert read more here
        ]
