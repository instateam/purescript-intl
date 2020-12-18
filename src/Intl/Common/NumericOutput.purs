module Intl.Common.NumericOutput where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Foreign (F, Foreign, ForeignError(..), fail, readString)
import Simple.JSON as JSON

data NumericOutput
  = Always
  | Auto

derive instance eqNumericOutput ∷ Eq NumericOutput

derive instance genericNumericOutput ∷ Generic NumericOutput _

instance showNumericOutput ∷ Show NumericOutput where
  show = genericShow

print ∷ NumericOutput → String
print = case _ of
  Always → "always"
  Auto → "auto"

parse ∷ String → Maybe NumericOutput
parse = case _ of
  "always" → Just Always
  "auto" → Just Auto
  _ → Nothing

instance readForeignNumericOutput ∷ JSON.ReadForeign NumericOutput where
  readImpl = parseFromString <=< readString
    where
    parseFromString ∷ String → F NumericOutput
    parseFromString s = case parse s of
      Nothing → fail (ForeignError ("Invalid NumericOutput representation: " <> s))
      Just numericOutput → pure numericOutput

instance writeForeignNumericOutput ∷ JSON.WriteForeign NumericOutput where
  writeImpl = JSON.writeImpl <<< print
