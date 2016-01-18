import Char
import String
import Dict
import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Json exposing ((:=))
import Task exposing (..)
import User
import Neighbor
import OpinionPathGroup as OPG
import NearestOpinions as NOPG
import Relationship


-- VIEW
view : String -> Result String (User.Model, List Neighbor.Model) -> String -> Result String NOPG.Model -> Html
view uid result tid resultsTopic =
  div []
    [ css "css/style.css"
    , viewUser uid result
    , viewNearest tid resultsTopic ]


viewNearest : String -> Result String NOPG.Model -> Html
viewNearest tid result =
  let field =
        input
          [ placeholder "Topic ID"
          , value tid
          , on "input" targetValue (Signal.message topicId.address)
          ]
          []
      paths =
        case result of
          Err msg ->
              [ div [] [ text msg ] ]

          Ok nearestOPGs ->
              NOPG.view nearestOPGs

  in
      div [] (field :: paths)


viewUser : String -> Result String (User.Model, List Neighbor.Model) -> Html
viewUser string result =
  let field =
        input
          [ placeholder "User ID"
          , value string
          , on "input" targetValue (Signal.message userId.address)
          ]
          []

      messages =
        case result of
          Err msg ->
              [ div [] [ text msg ] ]

          Ok (user, neighbors) ->
              [ div []
                [ span [] [text (user.name ++ ", " ++ toString user.id)]
                , div [] (List.map Neighbor.view neighbors)
                ]
              ]
  in
      div [] (field :: messages)


css : String -> Html
css path =
  node "link" [rel "stylesheet", href path] []


viewNeighbors : List Neighbor.Model -> Html
viewNeighbors neighbors =
  div [] (List.map Neighbor.view neighbors)


main =
  Signal.map4 view userId.signal results.signal topicId.signal topicResults.signal


userId : Signal.Mailbox String
userId =
  Signal.mailbox ""


topicId : Signal.Mailbox String
topicId =
  Signal.mailbox ""


topicResults : Signal.Mailbox (Result String NOPG.Model)
topicResults =
  Signal.mailbox (Err "Enter a topic ID")


results : Signal.Mailbox (Result String (User.Model, List Neighbor.Model))
results =
  Signal.mailbox (Err "Please enter a valid user")


port requests : Signal (Task x ())
port requests =
  Signal.map lookupNeighbors userId.signal
    |> Signal.map (\task -> Task.toResult task `andThen` Signal.send results.address)


port requestsNearest : Signal (Task x ())
port requestsNearest =
  Signal.map2 lookupNearestOpinions userId.signal topicId.signal
    |> Signal.map (\task -> Task.toResult task `andThen` Signal.send topicResults.address)


lookupNeighbors : String -> Task String (User.Model, List Neighbor.Model)
lookupNeighbors rawId =
  let toUrl =
        if isNum rawId
          then succeed ("http://localhost:3714/api/user/" ++ rawId)
          else fail ("Please enter a valid user")
  in
      toUrl `andThen` (mapError errorString << Http.get userInfo)
      -- toUrl `andThen` (mapError (always "Not found :(") << Http.get userDecode)


lookupNearestOpinions : String -> String -> Task String NOPG.Model
lookupNearestOpinions user topic =
  let toUrl =
        if isNum user && isNum topic
          then succeed ("http://localhost:3714/api/user/" ++ user ++ "/topic/" ++ topic ++ "/opinions")
          else fail ("User-Topic combo is not valid")
  in
      toUrl `andThen` (mapError errorString << Http.get OPG.decoder)
      |> Task.map NOPG.init


isNum : String -> Bool
isNum n =
  not (String.isEmpty n) && String.all Char.isDigit n


userInfo : Json.Decoder (User.Model, List Neighbor.Model)
userInfo =
  Json.object2 (,)
    ("user" := User.decoder)
    ("neighbors" := Json.list Neighbor.decoder)



errorString : Http.Error -> String
errorString error =
  case error of
    Http.Timeout -> "timeout"
    Http.NetworkError -> "network error"
    Http.UnexpectedPayload msg -> msg
    Http.BadResponse int msg -> (toString int) ++ ": " ++ msg
