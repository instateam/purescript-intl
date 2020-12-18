-- https://unicode.org/reports/tr35/tr35-numbers.html#Numbering_Systems
-- https://unicode-org.github.io/cldr-staging/charts/37/by_type/core_data.numbering_systems.html
module Intl.Common.NumberingSystem where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Foreign (F, Foreign, ForeignError(..), fail, readString)
import Simple.JSON as JSON

data NumberingSystem
  = Arab
  | ArabExt
  | Bali
  | Beng
  | Deva
  | Fullwide
  | Gujr
  | Guru
  | HaniDec
  | Khmr
  | Knda
  | Laoo
  | Latn
  | Libm
  | Mlym
  | Mong
  | Mymr
  | Orya
  | TamlDec
  | Telu
  | Thai
  | Tibt

derive instance eqNumberingSystem ∷ Eq NumberingSystem

derive instance genericNumberingSystem ∷ Generic NumberingSystem _

instance showNumberingSystem ∷ Show NumberingSystem where
  show = genericShow

print ∷ NumberingSystem → String
print = case _ of
  Arab → "arab"
  ArabExt → "arabext"
  Bali → "bali"
  Beng → "beng"
  Deva → "deva"
  Fullwide → "fullwide"
  Gujr → "Gujr"
  Guru → "Guru"
  HaniDec → "hanidec"
  Khmr → "khmr"
  Knda → "knda"
  Laoo → "laoo"
  Latn → "latn"
  Libm → "libm"
  Mlym → "mlym"
  Mong → "mong"
  Mymr → "mymr"
  Orya → "orya"
  TamlDec → "tamldec"
  Telu → "telu"
  Thai → "thai"
  Tibt → "tibt"

parse ∷ String → Maybe NumberingSystem
parse = case _ of
  "arab" → Just Arab
  "arabext" → Just ArabExt
  "bali" → Just Bali
  "beng" → Just Beng
  "deva" → Just Deva
  "fullwide" → Just Fullwide
  "gujr" → Just Gujr
  "guru" → Just Guru
  "hanidec" → Just HaniDec
  "khmr" → Just Khmr
  "knda" → Just Knda
  "laoo" → Just Laoo
  "latn" → Just Latn
  "limb" → Just Libm
  "mlym" → Just Mlym
  "mong" → Just Mong
  "mymr" → Just Mymr
  "orya" → Just Orya
  "tamldec" → Just TamlDec
  "telu" → Just Telu
  "thai" → Just Thai
  "tibt" → Just Tibt
  _ → Nothing

instance readForeignNumberingSystem ∷ JSON.ReadForeign NumberingSystem where
  readImpl = parseFromString <=< readString
    where
    parseFromString ∷ String → F NumberingSystem
    parseFromString s = case parse s of
      Nothing → fail (ForeignError ("Invalid NumberingSystem representation: " <> s))
      Just numberingSystem → pure numberingSystem

instance writeForeignNumberingSystem ∷ JSON.WriteForeign NumberingSystem where
  writeImpl = JSON.writeImpl <<< print
