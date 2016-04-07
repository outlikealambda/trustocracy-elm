module Opinion.Browser
  ( Browser
  , Action
  , empty
  , init
  , update
  , view
  , navButton
  ) where


import Opinion.Opinion as Opinion exposing (Opinion)
import Opinion.Presenter as Presenter
import Routes
import Topic.Model exposing (Topic)


import Effects exposing (Effects)
import Html exposing (Html, div, text, br)
import Html.Attributes exposing (class)


type alias Browser =
  { opinions : List Opinion
  , opinionsFetched : Bool
  }

type Action
  = FetchComplete (List Opinion)


empty : Browser
empty =
  { opinions = []
  , opinionsFetched = False
  }


init : Topic -> (Browser, Effects Action)
init {id} =
  ( empty
  , Effects.map FetchComplete (Opinion.fetchAllByTopic id)
  )


update : Action -> Browser -> (Browser, Effects Action)
update action browser =
  case action of
    FetchComplete opinions ->
      ( { browser
        | opinionsFetched = True
        , opinions =
            opinions
              |> List.map Presenter.prepare
              |> List.sortBy .influence
              |> List.reverse
        }
      , Effects.none
      )


view : (Int -> Routes.Route) -> Browser -> Html
view routeBuilder {opinions} =
  div
    [ class "opinion-browser" ]
    (List.map (Presenter.viewCollapsed routeBuilder) opinions)


navButton : Browser -> Html
navButton {opinions, opinionsFetched} =
  let
    opinionCount = toString <| List.length opinions
  in
    if opinionsFetched then
      div
        [ class "browse fetched" ]
        [ text opinionCount
        , br [] []
        , text "Total Opinions" ]
    else
      div [] []
