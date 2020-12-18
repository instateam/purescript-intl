module Intl.Common.StringDateTimeFormat where

import Prelude
import Data.Maybe (Maybe(..))
import Foreign (F, Foreign, ForeignError(..), fail, readString)
import Simple.JSON as JSON

data StringDateTimeFormat
  = Long
  | Short
  | Narrow

derive instance eqStringDateTimeFormat ∷ Eq StringDateTimeFormat

instance showStringDateTimeFormat ∷ Show StringDateTimeFormat where
  show = case _ of
    Long → "Long"
    Short → "Short"
    Narrow → "Narrow"

print ∷ StringDateTimeFormat → String
print = case _ of
  Long → "long"
  Short → "short"
  Narrow → "narrow"

parse ∷ String → Maybe StringDateTimeFormat
parse = case _ of
  "long" → Just Long
  "short" → Just Short
  "narrow" → Just Narrow
  _ → Nothing

instance readForeignStringDateTimeFormat ∷ JSON.ReadForeign StringDateTimeFormat where
  readImpl = parseFromString <=< readString
    where
    parseFromString ∷ String → F StringDateTimeFormat
    parseFromString s = case parse s of
      Nothing → fail (ForeignError ("Invalid StringDateTimeFormat representation: " <> s))
      Just displayLength → pure displayLength

instance writeForeignStringDateTimeFormat ∷ JSON.WriteForeign StringDateTimeFormat where
  writeImpl = JSON.writeImpl <<< print
