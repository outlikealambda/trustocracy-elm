module Common.Tether exposing
  ( Tether
  , init
  , data
  , map
  , id
  , attach
  , detach
  )


type Tether t a
  = Disjoint a -- has no corresponding database id
  | Attached t a  -- has a corresponding database id of type a


init : a -> Tether t a
init = Disjoint


data : Tether t a -> a
data t =
  case t of
    Disjoint a ->
      a
    Attached _ a ->
      a


map : (a -> b) -> Tether t a -> Tether t b
map fn tether =
  case tether of
    Disjoint m ->
      Disjoint (fn m)
    Attached t m ->
      Attached t (fn m)


id : Tether t a -> Maybe t
id tether =
  case tether of
    Disjoint _ ->
      Nothing
    Attached t _ ->
      Just t


attach : t -> Tether t a -> Tether t a
attach t tether =
  case tether of
    Disjoint a ->
      Attached t a
    Attached _ a ->
      Attached t a


detach : Tether t a -> Tether t a
detach tether =
  case tether of
    Disjoint _ ->
      tether
    Attached _ a ->
      Disjoint a
