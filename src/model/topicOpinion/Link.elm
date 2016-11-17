module Model.TopicOpinion.Link exposing
  ( Link
  , UserLink
  , score
  )


import Model.Path exposing (Path)


type alias Link =
  { userLink : UserLink
  , score : Int
  }


type alias UserLink = List Path


score : UserLink -> Int
score l =
  List.map .score l
  |> List.foldl (+) 0
