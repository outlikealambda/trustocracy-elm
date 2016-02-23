module Test.Displayable
  ( AsDisplayable
  , Displayable
  , Action
  , empty
  , init
  , update
  , view
  ) where


import Test.Opinion as Opinion exposing (Opinion)


import Effects exposing (Effects)
import Html exposing (Html, div, text, p)
import Html.Attributes exposing (class)
import Markdown
import String


type alias AsDisplayable m =
  { m
  | opinion: Opinion
  , displayable: Displayable
  }


type alias Displayable =
  { isExpanded : Bool
  , snippet : String
  }


type Action
  = Expand
  | Collapse


init : Bool -> String -> Displayable
init expanded text =
  { isExpanded = expanded
  , snippet = snippetize 200 text
  }


empty : Displayable
empty =
  { isExpanded = False
  , snippet = ""
  }


update : Action -> AsDisplayable m -> (AsDisplayable m, Effects Action)
update action model =
  case action of
    Expand ->
      let
        update =
          updateDisplayable expand model
      in
       ( update, Effects.none )

    Collapse ->
      let
        update =
          updateDisplayable collapse model
      in
        ( update, Effects.none )



updateDisplayable : (Displayable -> Displayable) -> AsDisplayable m -> AsDisplayable m
updateDisplayable transform model =
  { model | displayable = transform <| .displayable model }


expand : Displayable -> Displayable
expand = setExpanded True


collapse : Displayable -> Displayable
collapse = setExpanded False


setExpanded : Bool -> Displayable -> Displayable
setExpanded isIt d = { d | isExpanded = isIt }


isExpanded : AsDisplayable m -> Bool
isExpanded = .isExpanded << .displayable


view : AsDisplayable m -> Html
view {opinion, displayable} =
  let
    v =
      if displayable.isExpanded then
        viewFull opinion
      else
        viewSnippet displayable

    fetched =
      if opinion.fetched then "fetched" else ""

  in
    div
      [ class <| String.join " " ["opinion", fetched] ]
      [ v ]



viewFull : Opinion -> Html
viewFull opinion =
  div [ class "text markdown"] [ Markdown.toHtml opinion.text ]


viewSnippet : Displayable -> Html
viewSnippet displayable =
  div [ class "text snippet" ]
    [ p [] [ text displayable.snippet ]
    ]


snippetize : Int -> String -> String
snippetize maxLength s =
  -- add a space on the end to avoid removing the last word every time
  s ++ " "
    |> String.indexes " "
    |> maxValLessThan maxLength
    |> maybeSlice s


maxValLessThan : Int -> List Int -> Maybe Int
maxValLessThan maxVal ns =
  List.filter ((>) maxVal) ns
    |> List.maximum


maybeSlice : String -> Maybe Int -> String
maybeSlice s maybeBound =
  case maybeBound of
    Nothing -> s
    Just bound ->
      if bound < String.length s then
        (String.slice 0 bound s) ++ "..."
      else
        s
