module Intl.Common.NumericLength where

import Prelude
import Data.Maybe (Maybe(..))
import Foreign (F, Foreign, ForeignError(..), fail, readString)
import Simple.JSON as JSON

data NumericLength
  = Numeric
  | TwoDigit

derive instance eqNumericLength ∷ Eq NumericLength

instance showNumericLength ∷ Show NumericLength where
  show = case _ of
    Numeric → "Numeric"
    TwoDigit → "TwoDigit"

print ∷ NumericLength → String
print = case _ of
  Numeric → "numeric"
  TwoDigit → "2-digit"

parse ∷ String → Maybe NumericLength
parse = case _ of
  "numeric" → Just Numeric
  "2-digit" → Just TwoDigit
  _ → Nothing

instance readForeignNumericLength ∷ JSON.ReadForeign NumericLength where
  readImpl = parseFromString <=< readString
    where
    parseFromString ∷ String → F NumericLength
    parseFromString s = case parse s of
      Nothing → fail (ForeignError ("Invalid NumericLength representation: " <> s))
      Just numericLength → pure numericLength

instance writeForeignNumericLength ∷ JSON.WriteForeign NumericLength where
  writeImpl = JSON.writeImpl <<< print
