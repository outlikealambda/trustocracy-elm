module Utils.Date exposing
  ( asString
  )


import Date exposing (Date, Month (..))
import String


asString : Date -> String
asString date =
  String.concat
  [ toMonth date
  , " "
  , toString <| Date.day date
  , ", "
  , toString <| Date.year date
  ]


toMonth : Date -> String
toMonth date =
  case Date.month date of
    Jan ->
      "January"
    Feb ->
      "February"
    Mar ->
      "March"
    Apr ->
      "April"
    May ->
      "May"
    Jun ->
      "June"
    Jul ->
      "July"
    Aug ->
      "August"
    Sep ->
      "September"
    Oct ->
      "October"
    Nov ->
      "November"
    Dec ->
      "December"
