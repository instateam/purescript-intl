module Intl.Common.HourCycle where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Foreign (F, ForeignError(..), fail, readString)
import Simple.JSON as JSON

data HourCycle
  = H11
  | H12
  | H23
  | H24

derive instance eqHourCycle ∷ Eq HourCycle

derive instance genericHourCycle ∷ Generic HourCycle _

instance showHourCycle ∷ Show HourCycle where
  show = genericShow

print ∷ HourCycle → String
print = case _ of
  H11 → "h11"
  H12 → "h12"
  H23 → "h23"
  H24 → "h24"

parse ∷ String → Maybe HourCycle
parse = case _ of
  "h11" → Just H11
  "h12" → Just H12
  "h23" → Just H23
  "h24" → Just H24
  _ → Nothing

instance readForeignHourCycle ∷ JSON.ReadForeign HourCycle where
  readImpl = parseFromString <=< readString
    where
    parseFromString ∷ String → F HourCycle
    parseFromString s = case parse s of
      Nothing → fail (ForeignError ("Invalid HourCycle representation: " <> s))
      Just hourCycle → pure hourCycle

instance writeForeignHourCycle ∷ JSON.WriteForeign HourCycle where
  writeImpl = JSON.writeImpl <<< print
