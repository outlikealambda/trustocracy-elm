module Opinion.Reader
  ( Reader
  , Action
  , empty
  , init
  , update
  , view
  ) where


import ActiveUser exposing
  ( ActiveUser
    ( LoggedOut
    , LoggedIn
    )
  )
import Opinion.Opinion as Opinion exposing (Opinion)
import Opinion.Presenter as Presenter exposing (Presenter)


import Effects exposing (Effects)
import Html exposing (Html)


type alias Reader =
  { opinion: Opinion
  , activeUser : ActiveUser
  }


type Action
  = FetchComplete Opinion


empty : Reader
empty =
  { opinion = Opinion.empty
  , activeUser = LoggedOut
  }


init : ActiveUser -> Int -> (Reader, Effects Action)
init activeUser opinionId=
  let
    (opinion, opinionFx) =
      Opinion.fetchById opinionId
  in
    ( { activeUser = activeUser
      , opinion = opinion
      }
    , Effects.map FetchComplete opinionFx
    )


update : Action -> Reader -> (Reader, Effects Action)
update action reader =
  case action of
    FetchComplete opinion ->
      ( { reader | opinion = Presenter.expand opinion }
      , Effects.none
      )


view : Reader -> Html
view {activeUser, opinion} =
  case activeUser of
    LoggedOut ->
      Presenter.viewExpanded opinion
    LoggedIn _ ->
      Presenter.viewExpanded opinion
