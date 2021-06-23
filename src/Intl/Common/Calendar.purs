module Intl.Common.Calendar where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Foreign (F, ForeignError(..), fail, readString)
import Simple.JSON as JSON

data Calendar
  = Buddhist
  | Chinese
  | Coptic
  | Ethiopia
  | Gregory
  | Hebrew
  | Indian
  | Islamic
  | ISO8601
  | Japanese
  | Persian
  | Roc

derive instance eqCalendar ∷ Eq Calendar

derive instance genericCalendar ∷ Generic Calendar _

instance showCalendar ∷ Show Calendar where
  show = genericShow

print ∷ Calendar → String
print = case _ of
  Buddhist → "buddhist"
  Chinese → "chinese"
  Coptic → "coptic"
  Ethiopia → "ethiopia"
  Gregory → "gregory"
  Hebrew → "hebrew"
  Indian → "indian"
  Islamic → "islamic"
  ISO8601 → "iso8601"
  Japanese → "japanese"
  Persian → "persian"
  Roc → "roc"

parse ∷ String → Maybe Calendar
parse = case _ of
  "buddhist" → Just Buddhist
  "chinese" → Just Chinese
  "coptic" → Just Coptic
  "ethiopia" → Just Ethiopia
  "gregory" → Just Gregory
  "hebrew" → Just Hebrew
  "indian" → Just Indian
  "islamic" → Just Islamic
  "iso8601" → Just ISO8601
  "japanese" → Just Japanese
  "persian" → Just Persian
  "roc" → Just Roc
  _ → Nothing

instance readForeignCalendar ∷ JSON.ReadForeign Calendar where
  readImpl = parseFromString <=< readString
    where
    parseFromString ∷ String → F Calendar
    parseFromString s = case parse s of
      Nothing → fail (ForeignError ("Invalid Calendar representation: " <> s))
      Just calendar → pure calendar

instance writeForeignCalendar ∷ JSON.WriteForeign Calendar where
  writeImpl = JSON.writeImpl <<< print
