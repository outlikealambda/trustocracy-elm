module Model.Connection.Link exposing
  ( Link
  , UserLink
  )


import Model.Path exposing (Path)


type alias Link =
  { userLink : UserLink
  , score : Int
  }


type alias UserLink = List Path
