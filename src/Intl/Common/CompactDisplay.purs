module Intl.Common.CompactDisplay where

import Prelude
import Data.Maybe (Maybe(..))
import Foreign (F, Foreign, ForeignError(..), fail, readString)
import Simple.JSON as JSON

data CompactDisplay
  = Long
  | Short
  | Narrow

derive instance eqCompactDisplay ∷ Eq CompactDisplay

instance showCompactDisplay ∷ Show CompactDisplay where
  show = case _ of
    Long → "Long"
    Short → "Short"
    Narrow → "Narrow"

print ∷ CompactDisplay → String
print = case _ of
  Long → "long"
  Short → "short"
  Narrow → "narrow"

parse ∷ String → Maybe CompactDisplay
parse = case _ of
  "long" → Just Long
  "short" → Just Short
  "narrow" → Just Narrow
  _ → Nothing

instance readForeignCompactDisplay ∷ JSON.ReadForeign CompactDisplay where
  readImpl = parseFromString <=< readString
    where
    parseFromString ∷ String → F CompactDisplay
    parseFromString s = case parse s of
      Nothing → fail (ForeignError ("Invalid CompactDisplay representation: " <> s))
      Just displayLength → pure displayLength

instance writeForeignCompactDisplay ∷ JSON.WriteForeign CompactDisplay where
  writeImpl = JSON.writeImpl <<< print
