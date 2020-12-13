-- https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Intl/DateTimeFormat
module Intl.DateTimeFormat
  ( DateTimeFormat
  , create
  , DateTimeFormatOptions
  , supportedLocalesOf
  , SupportedLocalesOfOptions
  , format
  --, formatToParts
  , formatRange
  --, formatRangeToParts
  , module Intl.Common.DisplayLength
  , module Intl.Common.LocaleTag
  , module Intl.Common.TimeZone
  ) where

import Prelude

import Data.DateTime (DateTime)
import Data.Function.Uncurried (Fn2, Fn3, runFn2, runFn3)
import Intl.Common.DisplayLength (DisplayLength(..))
import Intl.Common.LocaleMatcher (LocaleMatcher(..))
import Intl.Common.LocaleTag (LocaleTag(..), localesToForeign)
import Intl.Common.LocaleTag as LocaleTag
import Intl.Common.TimeZone (TimeZone(..))
import Data.JSDate (JSDate)
import Data.JSDate as JSDate
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap, unwrap)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn4, runEffectFn1, runEffectFn2, runEffectFn4)
import Foreign (Foreign)
import Option (Option, recordFromRecord, recordToRecord)
import Option as Option
import Prim (kind Type, Array, Boolean, Record, String)
import Simple.JSON as JSON

foreign import data DateTimeFormat ∷ Type

---- Constructor ---------------------------------------------------------------
foreign import createImpl ∷ EffectFn2 Foreign Foreign DateTimeFormat

type DateTimeFormatOptions
  = ( dateStyle ∷ DisplayLength
    , timeStyle ∷ DisplayLength
    , calendar ∷ String -- Calendar
    , dayPeriod ∷ String -- DayPeriod
    , numberingSystem ∷ String -- NumeringSystem
    , localeMatcher ∷ LocaleMatcher
    , timeZone ∷ TimeZone
    , hour12 ∷ Boolean
    , hourCycle ∷ String -- HourCycle
    , formatMatcher ∷ String -- FormatMatcher
    )

create ∷
  ∀ record.
  Option.FromRecord record () DateTimeFormatOptions ⇒
  Array LocaleTag →
  Record record →
  Effect DateTimeFormat
create localeTags options'' = runEffectFn2 createImpl locales (JSON.write options)
  where
  locales ∷ Foreign
  locales = LocaleTag.localesToForeign localeTags

  options' ∷ Option.Record () DateTimeFormatOptions
  options' = Option.recordFromRecord options''

  options ∷ Record
    ( dateStyle ∷ Maybe DisplayLength
    , timeStyle ∷ Maybe DisplayLength
    , calendar ∷ Maybe String -- Calendar
    , dayPeriod ∷ Maybe String -- DayPeriod
    , numberingSystem ∷ Maybe String -- NumeringSystem
    , localeMatcher ∷ Maybe LocaleMatcher
    , timeZone ∷ Maybe TimeZone
    , hour12 ∷ Maybe Boolean
    , hourCycle ∷ Maybe String -- HourCycle
    , formatMatcher ∷ Maybe String -- FormatMatcher
    )
  options = Option.recordToRecord options'


---- Static methods ------------------------------------------------------------
foreign import supportedLocalesOfImpl ∷ EffectFn2 (Array String) Foreign (Array String)

type SupportedLocalesOfOptions
  = ( localeMatcher ∷ LocaleMatcher
    )

-- | Returns an array containing those of the provided locales that are
-- | supported without having to fall back to the runtime’s default locale.
supportedLocalesOf ∷
  ∀ record.
  Option.FromRecord record () SupportedLocalesOfOptions ⇒
  Array LocaleTag →
  Record record →
  Effect (Array LocaleTag)
supportedLocalesOf localeTags options'' = do
  locs ← runEffectFn2 supportedLocalesOfImpl locales (JSON.write options)
  pure (map wrap locs)
  where
  locales ∷ Array String
  locales = map unwrap localeTags

  options' ∷ Option.Record () SupportedLocalesOfOptions
  options' = Option.recordFromRecord options''

  options ∷ Record (localeMatcher ∷ Maybe LocaleMatcher )
  options = Option.recordToRecord options'

---- Instance methods ----------------------------------------------------------
foreign import formatImpl ∷ Fn2 JSDate DateTimeFormat String

-- | Getter function that formats a date according to the locale and formatting
-- | options of this `DateTimeFormat` object.
format ∷ JSDate → DateTimeFormat → String
format = runFn2 formatImpl

formatDateTime ∷ DateTime → DateTimeFormat → String
formatDateTime dateTime = format (JSDate.fromDateTime dateTime)

--foreign import formatToPartsImpl ∷ Fn2 JSDate DateTimeFormat (Array String)
-- | Returns an `Array` of objects representing the date string in parts that
-- | can be used for custom locale-aware formatting.
--formatToParts ∷ JSDate → DateTimeFormat → Array String
--formatToParts = runFn2 formatToPartsImpl
-- resolvedOptionsImpl
-- resolvedOptions
foreign import formatRangeImpl ∷ Fn3 JSDate JSDate DateTimeFormat String

-- | This method receives two `JSDate`s and formats the date range in the most
-- | concise way based on the locale and options provided when instantiating
-- | `DateTimeFormat`.
formatRange ∷ JSDate → JSDate → DateTimeFormat → String
formatRange = runFn3 formatRangeImpl

--formatRangeToPartsImpl ∷ Fn3 JSDate JSDate DateTimeFormat String
-- | This method receives two `JSDate`s and returns an `Array` of `Record`s
-- | containing the locale-specific tokens representing each part of the
-- | formatted date range.
