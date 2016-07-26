module Update.Whereabouts exposing (..)

import Common.API as Api

import Update.Place as PlaceUpdate

import Model.Whereabouts as Whereabouts exposing (Whereabouts)
import Model.Place as Place exposing (Place)


type Msg
  = PlacesFailedFetch String
  | PlacesCompletedFetch (List Place)
  | Modify Int PlaceUpdate.Msg
  | AddPlace
  | AddFailed String
  | AddComplete (List Place)

init : (Whereabouts, Cmd Msg)
init =
  ([], Api.fetchPlaces PlacesFailedFetch PlacesCompletedFetch )


modifyHelp : Int -> PlaceUpdate.Msg -> Place -> ( Maybe Place, Cmd Msg)
modifyHelp targetId msg place =

  if place.id /= targetId then
    ( Just place, Cmd.none )
  else
    let
      ( maybeNewPlace, cmd ) =
        PlaceUpdate.update msg place
    in
      ( maybeNewPlace
      , Cmd.map (Modify targetId) cmd
      )


update : Msg -> Whereabouts -> (Whereabouts, Cmd Msg)
update msg places =
  case msg of
    Modify givenId msg ->
      let
        ( initialPlaces, cmds ) =
            List.unzip (List.map (modifyHelp givenId msg) places)
        newPlaces =
          List.filterMap identity initialPlaces
      in
        ( newPlaces
        , Cmd.batch cmds)

    AddPlace ->
      ( places
      , Api.addPlace AddFailed AddComplete)

    AddFailed _ ->
      ( places, Debug.log "failed to add" Cmd.none )

    AddComplete addedPlace ->
      let
        newPlaces =
          ( List.append addedPlace places )
      in
        ( newPlaces
        , Debug.log "completed add" Cmd.none
        )

    PlacesFailedFetch err ->
      ( [], Debug.log ("failed to fetch places" ++ toString err) Cmd.none )

    PlacesCompletedFetch currentPlaces ->
      ( currentPlaces, Cmd.none )
