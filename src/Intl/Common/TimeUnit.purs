module Intl.Common.TimeUnit where

import Prelude
import Data.Maybe (Maybe(..))
import Foreign (F, Foreign, ForeignError(..), fail, readString)
import Simple.JSON as JSON

data TimeUnit
  = Year
  | Quarter
  | Month
  | Week
  | Day
  | Hour
  | Minute
  | Second

derive instance eqTimeUnit ∷ Eq TimeUnit

instance showTimeUnit ∷ Show TimeUnit where
  show = case _ of
    Year → "Year"
    Quarter → "Quarter"
    Month → "Month"
    Week → "Week"
    Day → "Day"
    Hour → "Hour"
    Minute → "Minute"
    Second → "Second"

print ∷ TimeUnit → String
print = case _ of
  Year → "year"
  Quarter → "quarter"
  Month → "month"
  Week → "week"
  Day → "day"
  Hour → "hour"
  Minute → "minute"
  Second → "second"

parse ∷ String → Maybe TimeUnit
parse = case _ of
  "year" → Just Year
  "quarter" → Just Quarter
  "month" → Just Month
  "week" → Just Week
  "day" → Just Day
  "hour" → Just Hour
  "minute" → Just Minute
  "second" → Just Second
  _ → Nothing

instance readForeignTimeUnit ∷ JSON.ReadForeign TimeUnit where
  readImpl = parseFromString <=< readString
    where
    parseFromString ∷ String → F TimeUnit
    parseFromString s = case parse s of
      Nothing → fail (ForeignError ("Invalid TimeUnit representation: " <> s))
      Just timeUnit → pure timeUnit

instance writeForeignTimeUnit ∷ JSON.WriteForeign TimeUnit where
  writeImpl = JSON.writeImpl <<< print
