module Intl.Common.TimeZone where

import Prelude
import Data.Newtype (class Newtype, unwrap, wrap)
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
