module Intl.Common.NumericDateTimeFormat where

import Prelude
import Data.Maybe (Maybe(..))
import Foreign (F, Foreign, ForeignError(..), fail, readString)
import Simple.JSON as JSON

data NumericDateTimeFormat
  = Numeric
  | TwoDigit

derive instance eqNumericDateTimeFormat ∷ Eq NumericDateTimeFormat

instance showNumericDateTimeFormat ∷ Show NumericDateTimeFormat where
  show = case _ of
    Numeric → "Numeric"
    TwoDigit → "TwoDigit"

print ∷ NumericDateTimeFormat → String
print = case _ of
  Numeric → "numeric"
  TwoDigit → "2-digit"

parse ∷ String → Maybe NumericDateTimeFormat
parse = case _ of
  "numeric" → Just Numeric
  "2-digit" → Just TwoDigit
  _ → Nothing

instance readForeignNumericDateTimeFormat ∷ JSON.ReadForeign NumericDateTimeFormat where
  readImpl = parseFromString <=< readString
    where
    parseFromString ∷ String → F NumericDateTimeFormat
    parseFromString s = case parse s of
      Nothing → fail (ForeignError ("Invalid NumericDateTimeFormat representation: " <> s))
      Just numericLength → pure numericLength

instance writeForeignNumericDateTimeFormat ∷ JSON.WriteForeign NumericDateTimeFormat where
  writeImpl = JSON.writeImpl <<< print
