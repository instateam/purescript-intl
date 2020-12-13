module Test.RelativeTimeFormat where

import Prelude

import Data.Either (Either(..), isLeft, isRight)
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (try)
import Intl.Common.DisplayLength as DisplayLength
import Intl.Common.LocaleTag (LocaleTag(..))
import Intl.Common.NumericLength as NumericLength
import Intl.Common.NumericOutput as NumericOutput
import Intl.Common.TimeUnit (TimeUnit(..))
import Intl.RelativeTimeFormat as RelativeTimeFormat
import Test.Assert (assert', assertEqual)

enUSLocale ∷ Array LocaleTag
enUSLocale = [ LocaleTag "en-US", LocaleTag "en" ]

esMXLocale ∷ Array LocaleTag
esMXLocale = [ LocaleTag "es-MX", LocaleTag "es" ]

shouldEqual ∷ ∀ a. Eq a ⇒ Show a ⇒ a → a → Effect Unit
shouldEqual a b =
  assertEqual { actual: a, expected: b}

main ∷ Effect Unit
main = do
  log "RelativeTimeFormat constructor"
    *> do
        emptyFormatter ← try $ RelativeTimeFormat.create [] {}
        assert' "Empty constructor failed" $ isRight emptyFormatter
        emptyTagFormatter ← try $ RelativeTimeFormat.create [ LocaleTag "" ] {}
        assert' "Empty tag should fail" $ isLeft emptyTagFormatter
  log "RelativeTimeFormat format"
    *> do
        enUSFormatter ← RelativeTimeFormat.create enUSLocale { style: DisplayLength.Narrow }
        RelativeTimeFormat.format 3.0 Quarter enUSFormatter `shouldEqual`  "in 3 qtrs."
        RelativeTimeFormat.format (-1.0) Day enUSFormatter `shouldEqual` "1 day ago"
        esMXFormatter ← RelativeTimeFormat.create esMXLocale { numeric: NumericOutput.Auto }
        RelativeTimeFormat.format 2.0 Day esMXFormatter `shouldEqual` "pasado mañana"
