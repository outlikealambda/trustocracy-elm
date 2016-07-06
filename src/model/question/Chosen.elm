module Model.Question.Chosen exposing
  ( Chosen
    ( None
    , Picked
    , Rated
    )
  )


type Chosen
  = None
  | Picked Int
  | Rated (List (Int, Float))
