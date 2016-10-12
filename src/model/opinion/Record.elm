module Model.Opinion.Record exposing
  ( Record
  )


import Model.Qualifications as Qualifications exposing (Qualifications)
import Model.Trustee as Trustee exposing (Trustee)


import Date exposing (Date)


-- even though record is only ever instantiated with an empty a,
-- we need it to create functions that work with records with contributed
-- fields; otherwise the type matching will fail.
type alias Record a =

  -- from API
  { a
  | text : String
  , author : Trustee
  , qualifications : Qualifications
  , created : Date

  -- derived
  , snippet : String
  , preview : String
  , expanded : Bool
  , fetched : Bool
  }
