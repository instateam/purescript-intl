module Test.DateTimeFormat where

import Prelude
import Data.Either (isLeft, isRight)
import Data.JSDate as JSDate
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (try)
import Intl.Common.Calendar as Calendar
import Intl.Common.LocaleTag (LocaleTag(..))
import Intl.Common.MonthDateTimeFormat as MonthDateTimeFormat
import Intl.Common.NumberingSystem as NumberingSystem
import Intl.Common.NumericDateTimeFormat as NumericDateTimeFormat
import Intl.Common.StringDateTimeFormat as StringDateTimeFormat
import Intl.DateTimeFormat as DateTimeFormat
import Test.Assert (assert', assertEqual)

shouldEqual ∷ ∀ a. Eq a ⇒ Show a ⇒ a → a → Effect Unit
shouldEqual actual expected = assertEqual { actual, expected }

main ∷ Effect Unit
main = do
  ----------------------------------------------------------------------------
  log "DateTimeFormat constructor"
    *> do
        emptyFormatter ← try $ DateTimeFormat.create [] {}
        assert' "Empty contrustor failed" $ isRight emptyFormatter
        emptyTagFormatter ← try $ DateTimeFormat.create [ LocaleTag "" ] {}
        assert' "Empty tag should fail" $ isLeft emptyTagFormatter
  ----------------------------------------------------------------------------
  log "DateTimeFormat supportedLocalesOf"
    *> do
        -- Toki Pona likely won’t be supported :(
        supported ← DateTimeFormat.supportedLocalesOf [ LocaleTag "mis", LocaleTag "en" ] {}
        supported `shouldEqual` [ LocaleTag "en" ]
  ----------------------------------------------------------------------------
  log "DateTimeFormat format"
    *> do
        enUSBaseFormatter ← DateTimeFormat.create enUSLocale {}
        DateTimeFormat.format ripGeorgeFloyd enUSBaseFormatter
          `shouldEqual`
            "5/25/2020"
        thTHBaseFormatter ← DateTimeFormat.create thTHLocale {}
        DateTimeFormat.format ripGeorgeFloyd thTHBaseFormatter
          `shouldEqual`
            "25/5/2563"
        loLANumbersISOYearsFormatter ←
          DateTimeFormat.create loLALocale
            { calendar: Calendar.ISO8601 -- Usually Buddhist
            , numberingSystem: NumberingSystem.Laoo
            , weekday: StringDateTimeFormat.Short
            , year: NumericDateTimeFormat.Numeric
            , month: MonthDateTimeFormat.from StringDateTimeFormat.Short
            , day: NumericDateTimeFormat.Numeric
            , minute: NumericDateTimeFormat.TwoDigit
            , second: NumericDateTimeFormat.TwoDigit
            }
        DateTimeFormat.format ripGeorgeFloyd loLANumbersISOYearsFormatter
          `shouldEqual`
            "ຈັນ, ໒໕ ພ.ພ. ໒໐໒໐, ໒໕:໐໐"
  ----------------------------------------------------------------------------
  where
  enUSLocale ∷ Array LocaleTag
  enUSLocale = [ LocaleTag "en-US", LocaleTag "en" ]

  thTHLocale ∷ Array LocaleTag
  thTHLocale = [ LocaleTag "th-TH", LocaleTag "th" ]

  loLALocale ∷ Array LocaleTag
  loLALocale = [ LocaleTag "lo-LA", LocaleTag "lo" ]

  -- JS UTC Date months start at index 0 as January
  ripGeorgeFloyd ∷ JSDate.JSDate
  ripGeorgeFloyd =
    JSDate.jsdate
      { year: 2020.0
      , month: 4.0
      , day: 25.0
      , hour: 14.0
      , minute: 25.0
      , second: 0.0
      , millisecond: 0.0
      }
