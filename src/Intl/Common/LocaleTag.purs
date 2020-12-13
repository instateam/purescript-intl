module Intl.Common.LocaleTag where

import Prelude
import Data.Newtype (class Newtype, unwrap, wrap)
import Foreign (F, Foreign, ForeignError(..), fail, readString)
import Simple.JSON as JSON

-- | Used to wrap the Unicode locale identifier string (e.g. `LocaleTag "en-US"`)
newtype LocaleTag
  = LocaleTag String

derive instance newtypeLocaleTag ∷ Newtype LocaleTag _

derive newtype instance eqLocaleTag ∷ Eq LocaleTag

derive newtype instance ordLocaleTag ∷ Ord LocaleTag

instance showLocaleTag ∷ Show LocaleTag where
  show (LocaleTag l) = "(LocaleTag " <> l <> ")"

instance readForeignLocaleTag ∷ JSON.ReadForeign LocaleTag where
  readImpl = map wrap <<< readString

instance writeForeignLocaleTag ∷ JSON.WriteForeign LocaleTag where
 writeImpl = JSON.writeImpl <<< unwrap

localesToForeign ∷ Array LocaleTag → Foreign
localesToForeign = case _ of
  [] → JSON.undefined
  [ l ] → JSON.write l
  ls → JSON.write ls
