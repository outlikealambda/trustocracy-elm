module Opinion.Model
  ( Model
  , ApiModel
  , empty
  , init
  , initExpanded
  , decoder
  , setText
  ) where


import String


import Opinion.Credentials as Credentials
import Json.Decode as Json exposing ((:=))


type alias Model =
  { oid: Int
  , expanded: Bool
  , text : String
  , snippet : String
  , credentials : Credentials.Model
  }


type alias ApiModel =
  { oid : Int
  , text : String
  }


empty : Model
empty =
  Model -1 False "" "" Credentials.init


init : Maybe ApiModel -> Model
init apiModel =
  case Debug.log "opinion initted" apiModel of
    Nothing -> empty
    Just {oid, text} ->
      Model oid False "" "" Credentials.init
        |> setText text


initExpanded : Maybe ApiModel -> Model
initExpanded = setExpanded << init


setExpanded : Model -> Model
setExpanded m =
  { m | expanded = True }


decoder : Json.Decoder ApiModel
decoder =
  Json.object2 ApiModel
    ( "id" := Json.int )
    ( "text" := Json.string )


setText : String -> Model -> Model
setText text model =
  { model
  | text = text
  , snippet = snippetize 200 text }


snippetize : Int -> String -> String
snippetize maxLength s =
  -- add a space on the end to avoid removing the last word every time
  s ++ " "
    |> String.indexes " "
    |> maxValLessThan maxLength
    |> maybeSlice s


maybeSlice : String -> Maybe Int -> String
maybeSlice s maybeBound =
  case maybeBound of
    Nothing -> s
    Just bound ->
      if bound < String.length s then
        (String.slice 0 bound s) ++ "..."
      else
        s


maxValLessThan : Int -> List Int -> Maybe Int
maxValLessThan maxVal ns =
  List.filter ((>) maxVal) ns
    |> List.maximum
