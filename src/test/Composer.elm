module Test.Composer
  ( Composer
  , Action
  , empty
  , init
  , update
  , view
  ) where


import Test.Opinion as Opinion exposing (Opinion)
import Test.Displayable as Displayable exposing (Displayable)
import Test.Writable as Writable
import User as User exposing (User)
import Topic.Model as Topic exposing (Topic)


import Effects exposing (Effects)
import Html exposing (Html, input, div, textarea, text, h3, button)
import Html.Attributes exposing (class, placeholder, value)


type alias Composer
  = Writable.AsWritable
    ( Displayable.AsDisplayable
      { user: User
      , topic: Topic
      }
    )


type Action
  = FetchComplete Opinion
  | WriteMsg Writable.Action


empty : Composer
empty =
  { opinion = Opinion.empty
  , displayable = Displayable.empty
  , user = User.empty
  , topic = Topic.empty
  }


init : User -> Topic -> (Composer, Effects Action)
init user topic =
  ( { empty
    | user = user
    , topic = topic
    }
  , Opinion.fetchByUserTopic user.id topic.id
    |> Effects.map FetchComplete
  )


updateWritable : Writable.OnUpdate Composer
updateWritable =
  { write = writeText
  }


update : Action -> Composer -> (Composer, Effects Action)
update action composer =
  case action of

    FetchComplete opinion ->
      ( { composer
        | opinion = opinion
        , displayable = Displayable.init True opinion.text
        }
      , Effects.none )

    WriteMsg message ->
      let
        (update, updateFx) =
          Writable.update message updateWritable composer
      in
        ( update
        , Effects.map WriteMsg updateFx )


view : Signal.Address Action -> Composer -> Html
view address composer =
  div [ class "row composer" ]
    [ div [ class "col m12 l6" ]
      [ div [ class "t-card" ]
        [ div [ class "t-card-body" ]
          [ div [ class "subtitle" ] [ text "Write" ]
          , Writable.view (Signal.forwardTo address WriteMsg) (.opinion >> .text) composer
          ]
        ]
      ]
    , div [ class "col m12 l6 preview" ]
      [ div [ class "t-card" ]
        [ div [ class "t-card-body" ]
          [ div [ class "subtitle" ] [ text "Preview" ]
          , Displayable.view composer
          ]
        ]
      ]
    ]


-- private
getOpinion : Composer -> Opinion
getOpinion = .opinion


-- write text to opinion from writable
writeText : Composer -> String -> Composer
writeText composer t =
  { composer
  | opinion = Opinion.setText (getOpinion composer) t
  }
