module Intl.RelativeTimeFormat.NumericFormat where

import Prelude
import Data.Maybe (Maybe(..))
import Foreign (F, ForeignError(..), fail, readString)
import Simple.JSON as JSON

data NumericFormat
  = Always
  | Auto

derive instance eqNumericFormat ∷ Eq NumericFormat

instance showNumericFormat ∷ Show NumericFormat where
  show = case _ of
    Always → "Always"
    Auto → "Auto"

print ∷ NumericFormat → String
print = case _ of
  Always → "always"
  Auto → "auto"

parse ∷ String → Maybe NumericFormat
parse = case _ of
  "always" → Just Always
  "auto" → Just Auto
  _ → Nothing

instance readForeignNumericFormat ∷ JSON.ReadForeign NumericFormat where
  readImpl = parseFromString <=< readString
    where
    parseFromString ∷ String → F NumericFormat
    parseFromString s = case parse s of
      Nothing → fail (ForeignError ("Invalid NumericFormat representation: " <> s))
      Just numericFormat → pure numericFormat

instance writeForeignNumericFormat ∷ JSON.WriteForeign NumericFormat where
  writeImpl = JSON.writeImpl <<< print
