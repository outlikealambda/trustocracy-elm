module Update.Place exposing (..)

import Model.Place as Place exposing (Place)

import Common.API as Api


type Msg
  = NameChange String
  | CountryChange String
  | CityChange String
  | PostalChange String
  | UpdatePlace
  | UpdateFailed String
  | UpdateComplete Place
  | RemovePlace
  | RemoveFailed String
  | RemoveComplete Int


update : Msg -> Place -> (Maybe Place, Cmd Msg)
update msg place =
  case msg of
    NameChange input ->
      Just { place | name = ( Debug.log "input" input) } ![]
    CountryChange input ->
      Just { place | country = ( Debug.log "input" input) } ![]
    CityChange input ->
      Just { place | city = ( Debug.log "input" input) } ![]
    PostalChange input ->
      Just { place | postal = ( Debug.log "input" input) } ![]
    UpdatePlace ->
      (Just place
      , Api.updatePlace UpdateFailed UpdateComplete place)
    UpdateFailed err ->
      (Just (Debug.log ("failed to update" ++ toString err) place), Cmd.none)
    UpdateComplete location ->
      (Just location
      , Debug.log "completed update" Cmd.none
      )
    RemovePlace ->
      (Just place
      , Api.removePlace RemoveFailed RemoveComplete place.id)
    RemoveFailed _ ->
      (Just place, Debug.log "failed to remove" Cmd.none)
    RemoveComplete locationId ->
      (Nothing, Debug.log "completed removal" Cmd.none)
