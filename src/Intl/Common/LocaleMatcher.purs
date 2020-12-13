module Intl.Common.LocaleMatcher where

import Prelude
import Data.Maybe (Maybe(..))
import Foreign (F, ForeignError(..), fail, readString)
import Simple.JSON as JSON

data LocaleMatcher
  = Lookup
  | BestFit

derive instance eqLocaleMatcher ∷ Eq LocaleMatcher

instance showLocaleMatcher ∷ Show LocaleMatcher where
  show = case _ of
    Lookup → "Lookup"
    BestFit → "BestFit"

print ∷ LocaleMatcher → String
print = case _ of
  Lookup → "lookup"
  BestFit → "best fit"

parse ∷ String → Maybe LocaleMatcher
parse = case _ of
  "lookup" → Just Lookup
  "best fit" → Just BestFit
  _ → Nothing

instance readForeignDisplayLength ∷ JSON.ReadForeign LocaleMatcher where
  readImpl = parseFromString <=< readString
    where
    parseFromString ∷ String → F LocaleMatcher
    parseFromString s = case parse s of
      Nothing → fail (ForeignError ("Invalid LocaleMatcher representation: " <> s))
      Just localeMatcher → pure localeMatcher

instance writeForeignDisplayLength ∷ JSON.WriteForeign LocaleMatcher where
  writeImpl = JSON.writeImpl <<< print
