module Opinion.Writer
  ( Writer
  , Action
  , update
  , view
  ) where


import Html exposing (Html, div, textarea)
import Html.Attributes exposing (class, placeholder, value)
import Html.Events exposing (on, targetValue)


import Qualifications exposing (Qualifications)


type alias Writer a =
  { a
    | text : String
    , qualifications : Qualifications
  }


type Action
  = Write String
  | QualificationsMsg Qualifications.Action


update : Action -> Writer a -> Writer a
update action writer =
  case action of
    Write raw ->
      { writer | text = raw }
    QualificationsMsg msg ->
      { writer
        | qualifications = Qualifications.update msg writer.qualifications }



view : Signal.Address Action -> Writer a -> Html
view address {text, qualifications} =
  div
    [ class "writer" ]
    [ viewOpinionInput address text
    , viewQualificationsInput address qualifications
    ]


viewOpinionInput : Signal.Address Action -> String -> Html
viewOpinionInput address currentText =
  div [ class "opinion-creator" ]
    [ div [ class "input-field" ]
      [ textarea
        [ class "write"
        , placeholder "Let's write something!"
        , value currentText
        , on "input" targetValue (Signal.message address << Write)
        ]
        []
      ]
    -- , div [ class "character-count" ]
    --   [ String.length currentText
    --     |> toString
    --     |> flip (++) " characters written"
    --     |> Html.text
    --   ]
    ]


viewQualificationsInput : Signal.Address Action -> Qualifications -> Html
viewQualificationsInput address qualifications =
  div
    [ class "write-qualifications" ]
    [ div
      [ class "section-header" ]
      [ Html.text "Topic Specific Qualifications" ]
    , div
      [ class "section-sub-header" ]
      [ Html.text "for general qualifications, edit your profile" ]
    , Qualifications.viewForm
      (Signal.forwardTo address QualificationsMsg)
      qualifications
    ]
