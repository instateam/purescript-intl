module Intl.Common.TimeZone where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Maybe (Maybe(..))
import Foreign (F, Foreign, ForeignError(..), fail, readString)
import Simple.JSON as JSON

-- | Used to wrap the time zone
newtype TimeZone
  = TimeZone String

derive instance newtypeTimeZone ∷ Newtype TimeZone _

derive instance eqTimeZone ∷ Eq TimeZone

instance showTimeZone ∷ Show TimeZone where
  show (TimeZone tz) = "(TimeZone " <> tz <> ")"

instance readForeignLocaleTag ∷ JSON.ReadForeign TimeZone where
  readImpl = map wrap <<< readString

instance writeForeignLocaleTag ∷ JSON.WriteForeign TimeZone where
  writeImpl = JSON.writeImpl <<< unwrap

data TimeZoneNameFormat
  = TimeZoneNameLong
  | TimeZoneNameShort

derive instance eqTimeZoneNameFormat ∷ Eq TimeZoneNameFormat

derive instance genericTimeZoneNameFormate ∷ Generic TimeZoneNameFormat _

instance showTimeZoneNameFormat ∷ Show TimeZoneNameFormat where
  show = genericShow

print ∷ TimeZoneNameFormat → String
print = case _ of
  TimeZoneNameLong → "long"
  TimeZoneNameShort → "short"

parse ∷ String → Maybe TimeZoneNameFormat
parse = case _ of
  "long" → Just TimeZoneNameLong
  "short" → Just TimeZoneNameShort
  _ → Nothing

instance readTimeZoneNameFormat ∷ JSON.ReadForeign TimeZoneNameFormat where
  readImpl = parseFromString <=< readString
    where
    parseFromString ∷ String → F TimeZoneNameFormat
    parseFromString s = case parse s of
      Nothing → fail (ForeignError ("Invalid TimeZoneNameFormat representation: " <> s))
      Just calendar → pure calendar

instance writeTimeZoneNameFormat ∷ JSON.WriteForeign TimeZoneNameFormat where
  writeImpl = JSON.writeImpl <<< print
