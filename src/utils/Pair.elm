module Utils.Pair exposing
  ( fstMap
  , sndMap
  )


fstMap : (a -> b) -> (a, c) -> (b, c)
fstMap fn (a, c) =
  (fn a, c)


sndMap : (a -> b) -> (c, a) -> (c, b)
sndMap fn (c, a) =
  (c, fn a)
