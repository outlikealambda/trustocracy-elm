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
import Topic.Model exposing (Topic)


import Effects exposing (Effects)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)


type alias Browser =
  { opinions : List Opinion
  }

type Action
  = FetchComplete (List Opinion)


empty : Browser
empty =
  { opinions = []
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
        | opinions =
            opinions
              |> List.map Presenter.prepare
              |> List.sortBy .influence
              |> List.reverse
        }
      , Effects.none
      )


view : Browser -> Html
view {opinions} =
  div
    [ class "opinion-browser" ]
    (List.map Presenter.view opinions)


navButton : Browser -> Html
navButton {opinions} =
  let
    count = List.length opinions
  in
    div
      [ class "browse" ]
      [ text <| (toString count) ++ " opinions" ]
