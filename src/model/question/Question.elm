module Model.Question.Question exposing
  ( Question
  , Selector (..)
  , decoder
  , samples
  , raters
  )


import Model.Question.Option as Option exposing (Option)


import Json.Decode as Decode


import Dict exposing (Dict)


type alias Question =
  { id : Int
  , prompt : String
  , promptShort : String
  , selector : Selector
  }


type Selector
  = Picker (List Option)
  | Rater (Option, Option)


raters : List Question -> Dict Int (Option, Option)
raters questions =
  let
    raterOptions question =
      case question.selector of
        Rater opts ->
          Just (question.id, opts)
        Picker _ ->
          Nothing
  in
    List.filterMap raterOptions questions
      |> Dict.fromList


decoder : Decode.Decoder Question
decoder =
  Decode.andThen typeCheckingDecoder (Decode.field "type" Decode.string)


typeCheckingDecoder : String -> Decode.Decoder Question
typeCheckingDecoder qType =
  case qType of
    "PICK" ->
      Decode.map4 pickerFromApi
        (Decode.field "id" Decode.int)
        (Decode.field "prompt" Decode.string)
        (Decode.field "promptShort" Decode.string)
        (Decode.at ["options", "answers"] <| Decode.list Option.decoder)
    "RATE" ->
      Decode.map4 raterFromApi
        (Decode.field "id" Decode.int)
        (Decode.field "prompt" Decode.string)
        (Decode.field "promptShort" Decode.string)
        (Decode.at ["options", "endpoints"] <|
          Decode.map2 (,)
            (Decode.index 0 Option.decoder)
            (Decode.index 1 Option.decoder))
    _ ->
      Decode.fail <| "we do not support " ++ qType ++ " questions yet"


pickerFromApi : Int -> String -> String -> List Option -> Question
pickerFromApi id prompt promptShort options =
  { id = id
  , prompt = prompt
  , promptShort = promptShort
  , selector = Picker options
  }


raterFromApi : Int -> String -> String -> (Option, Option) -> Question
raterFromApi id prompt promptShort endpoints =
  { id = id
  , prompt = prompt
  , promptShort = promptShort
  , selector = Rater endpoints
  }


samples : List Question
samples =
  [ { id = 1
    , prompt = "Do you prefer Soda A or Soda X"
    , promptShort = "Which Soda?"
    , selector = Picker
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
    }
  , { id = 2
    , prompt = "Would you like one hamburgah wit cheese, or one cheeseburgah"
    , promptShort = "Which Style?"
    , selector = Picker
      [ { id = 1
        , label = "Hamburger with cheese"
        }
      , { id = 2
        , label = "Cheeseburger"
        }
      ]
    }
  ]
