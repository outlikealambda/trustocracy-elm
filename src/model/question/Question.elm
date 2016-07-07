module Model.Question.Question exposing
  ( Question
  , Selector (..)
  , decoder
  , samples
  )


import Model.Question.Option as Option exposing (Option)


import Json.Decode as Decode exposing ((:=))


type alias Question =
  { id : Int
  , prompt : String
  , promptShort : String
  , options : List Option
  , selector : Selector
  }


type Selector
  = Picker
  | Rater


decoder : Decode.Decoder Question
decoder =
  ("type" := Decode.string) `Decode.andThen` typeCheckingDecoder


typeCheckingDecoder : String -> Decode.Decoder Question
typeCheckingDecoder qType =
  case qType of
    "PICK_ONE" ->
      Decode.object4 (fromApi Picker)
        ("id" := Decode.int)
        ("prompt" := Decode.string)
        ("promptShort" := Decode.string)
        (Decode.at ["options", "answers"] <| Decode.list Option.decoder)
    "ASSESS" ->
      Decode.fail "we do not support ASSESS questions yet"
    _ ->
      Decode.fail <| "we do not support " ++ qType ++ " questions yet"


fromApi : Selector -> Int -> String -> String -> List Option -> Question
fromApi selector id prompt promptShort options =
  { id = id
  , prompt = prompt
  , promptShort = promptShort
  , options = options
  , selector = selector
  }



samples : List Question
samples =
  [ { id = 1
    , prompt = "Do you prefer Soda A or Soda X"
    , promptShort = "Which Soda?"
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
    , promptShort = "Which Style?"
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
