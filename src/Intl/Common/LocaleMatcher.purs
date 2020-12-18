module Intl.Common.LocaleMatcher where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Foreign (F, ForeignError(..), fail, readString)
import Simple.JSON as JSON

data LocaleMatcher
  = Lookup
  | BestFit

derive instance eqLocaleMatcher ∷ Eq LocaleMatcher

derive instance genericLocaleMatcher ∷ Generic LocaleMatcher _

instance showLocaleLocaleMatcher ∷ Show LocaleMatcher where
  show = genericShow

print ∷ LocaleMatcher → String
print = case _ of
  Lookup → "lookup"
  BestFit → "best fit"

parse ∷ String → Maybe LocaleMatcher
parse = case _ of
  "lookup" → Just Lookup
  "best fit" → Just BestFit
  _ → Nothing

instance readForeignLocaleMatcher ∷ JSON.ReadForeign LocaleMatcher where
  readImpl = parseFromString <=< readString
    where
    parseFromString ∷ String → F LocaleMatcher
    parseFromString s = case parse s of
      Nothing → fail (ForeignError ("Invalid LocaleMatcher representation: " <> s))
      Just localeLocaleMatcher → pure localeLocaleMatcher

instance writeForeignLocaleMatcher ∷ JSON.WriteForeign LocaleMatcher where
  writeImpl = JSON.writeImpl <<< print
