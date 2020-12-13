module Intl.Common.DisplayLength where

import Prelude
import Data.Maybe (Maybe(..))
import Foreign (F, Foreign, ForeignError(..), fail, readString)
import Simple.JSON as JSON

data DisplayLength
  = Long
  | Short
  | Narrow

derive instance eqDisplayLength ∷ Eq DisplayLength

instance showDisplayLength ∷ Show DisplayLength where
  show = case _ of
    Long → "Long"
    Short → "Short"
    Narrow → "Narrow"

print ∷ DisplayLength → String
print = case _ of
  Long → "long"
  Short → "short"
  Narrow → "narrow"

parse ∷ String → Maybe DisplayLength
parse = case _ of
  "long" → Just Long
  "short" → Just Short
  "narrow" → Just Narrow
  _ → Nothing

instance readForeignDisplayLength ∷ JSON.ReadForeign DisplayLength where
  readImpl = parseFromString <=< readString
    where
    parseFromString ∷ String → F DisplayLength
    parseFromString s = case parse s of
      Nothing → fail (ForeignError ("Invalid DisplayLength representation: " <> s))
      Just displayLength → pure displayLength

instance writeForeignDisplayLength ∷ JSON.WriteForeign DisplayLength where
  writeImpl = JSON.writeImpl <<< print
