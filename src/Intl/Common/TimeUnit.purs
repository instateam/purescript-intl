module Intl.Common.TimeUnit where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Foreign (F, ForeignError(..), fail, readString)
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

derive instance genericTimeUnit ∷ Generic TimeUnit _

instance showTimeUnit ∷ Show TimeUnit where
  show = genericShow

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
  "years" → Just Year
  "quarter" → Just Quarter
  "quarters" → Just Quarter
  "month" → Just Month
  "months" → Just Month
  "week" → Just Week
  "weeks" → Just Week
  "day" → Just Day
  "days" → Just Day
  "hour" → Just Hour
  "hours" → Just Hour
  "minute" → Just Minute
  "minutes" → Just Minute
  "second" → Just Second
  "seconds" → Just Second
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
