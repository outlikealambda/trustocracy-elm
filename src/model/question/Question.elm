module Model.Question.Question exposing
  ( Question
  , Selector (..)
  , samples
  )


import Model.Question.Chosen as Chosen exposing (Chosen)
import Model.Question.Option exposing (Option)


type alias Question =
  { id : Int
  , prompt : String
  , chosen : Chosen
  , options : List Option
  , selector : Selector
  }


type Selector
  = Picker
  | Rater


samples : List Question
samples =
  [ { id = 1
    , prompt = "Do you prefer Soda A or Soda X"
    , chosen = Chosen.None
    , options =
      [ { id = 1
        , label = "Soda A"
        }
      , { id = 2
        , label = "Soda X"
        }
      , { id = 3
        , label = "da Cracka"
        }
      ]
    , selector = Picker
    }
  , { id = 2
    , prompt = "Would you like one hamburgah wit cheese, or one cheeseburgah"
    , chosen = Chosen.None
    , options =
      [ { id = 1
        , label = "Hamburger with cheese"
        }
      , { id = 2
        , label = "Cheeseburger"
        }
      ]
    , selector = Picker
    }
  ]
