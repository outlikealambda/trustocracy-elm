module Model.Opinion.Record exposing
  ( Record
  )


import Model.Qualifications as Qualifications exposing (Qualifications)
import Model.Trustee as Trustee exposing (Trustee)


import Date exposing (Date)


type alias Record a =

  -- from API
  { a
  | text : String
  , influence : Int
  , author : Trustee
  , qualifications : Qualifications
  , created : Date

  -- derived
  , snippet : String
  , preview : String
  , expanded : Bool
  , fetched : Bool
  }
