module Model.Question.Answer exposing
  ( Answer
    ( None
    , Picked
    , Rated
    )
  )


type Answer
  = None
  | Picked Int
  | Rated (List (Int, Float))
