-- https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Intl/DateTimeFormat
module Intl.DateTimeFormat
  ( DateTimeFormat
  , create
  , DateTimeFormatOptions
  , supportedLocalesOf
  , SupportedLocalesOfOptions
  , format
  , formatDateTime
  , formatToParts
  , formatRange
  --, formatRangeToParts
  , module Intl.Common.Calendar
  , module Intl.Common.HourCycle
  , module Intl.Common.LocaleTag
  , module Intl.Common.NumberingSystem
  , module Intl.Common.NumericDateTimeFormat
  , module Intl.Common.StringDateTimeFormat
  , module Intl.Common.TimeZone
  ) where

import Prelude
import Data.DateTime (DateTime)
import Data.Either (either)
import Data.Function.Uncurried (Fn2, Fn3, runFn2, runFn3)
import Data.JSDate (JSDate)
import Data.JSDate as JSDate
import Data.List.NonEmpty (toUnfoldable) as NEL
import Data.Maybe (Maybe)
import Data.Newtype (wrap, unwrap)
import Data.String (joinWith)
import Effect (Effect)
import Effect.Uncurried (EffectFn2, runEffectFn2)
import Foreign (Foreign, renderForeignError)
import Intl.Common.Calendar (Calendar(..))
import Intl.Common.FormattedDateParts (FormattedDateParts)
import Intl.Common.HourCycle (HourCycle(..))
import Intl.Common.LocaleMatcher (LocaleMatcher)
import Intl.Common.LocaleTag (LocaleTag(..), localesToForeign)
import Intl.Common.LocaleTag as LocaleTag
import Intl.Common.MonthDateTimeFormat (MonthDateTimeFormat)
import Intl.Common.NumberingSystem (NumberingSystem(..))
import Intl.Common.NumericDateTimeFormat (NumericDateTimeFormat(..))
import Intl.Common.StringDateTimeFormat (StringDateTimeFormat(..))
import Intl.Common.TimeZone (TimeZone(..), TimeZoneNameFormat(..))
import Option as Option
import Partial.Unsafe (unsafeCrashWith)
import Simple.JSON as JSON

foreign import data DateTimeFormat ∷ Type

---- Constructor ---------------------------------------------------------------
foreign import createImpl ∷ EffectFn2 Foreign Foreign DateTimeFormat

type DateTimeFormatOptions
  = ( dateStyle ∷ StringDateTimeFormat
    , timeStyle ∷ StringDateTimeFormat
    , calendar ∷ Calendar
    , dayPeriod ∷ StringDateTimeFormat
    , numberingSystem ∷ NumberingSystem
    , localeMatcher ∷ LocaleMatcher
    , timeZone ∷ TimeZone
    , hour12 ∷ Boolean
    , hourCycle ∷ HourCycle
    , formatMatcher ∷ LocaleMatcher
    , weekday ∷ StringDateTimeFormat
    , era ∷ StringDateTimeFormat
    , year ∷ NumericDateTimeFormat
    , month ∷ MonthDateTimeFormat
    , day ∷ NumericDateTimeFormat
    , hour ∷ NumericDateTimeFormat
    , minute ∷ NumericDateTimeFormat
    , second ∷ NumericDateTimeFormat
    , fractionalSecondDigit ∷ Int
    , timeZoneName ∷ TimeZoneNameFormat
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

  options ∷
    Record
      ( dateStyle ∷ Maybe StringDateTimeFormat
      , timeStyle ∷ Maybe StringDateTimeFormat
      , calendar ∷ Maybe Calendar
      , dayPeriod ∷ Maybe StringDateTimeFormat
      , numberingSystem ∷ Maybe NumberingSystem
      , localeMatcher ∷ Maybe LocaleMatcher
      , timeZone ∷ Maybe TimeZone
      , hour12 ∷ Maybe Boolean
      , hourCycle ∷ Maybe HourCycle
      , formatMatcher ∷ Maybe LocaleMatcher
      , weekday ∷ Maybe StringDateTimeFormat
      , era ∷ Maybe StringDateTimeFormat
      , year ∷ Maybe NumericDateTimeFormat
      , month ∷ Maybe MonthDateTimeFormat
      , day ∷ Maybe NumericDateTimeFormat
      , hour ∷ Maybe NumericDateTimeFormat
      , minute ∷ Maybe NumericDateTimeFormat
      , second ∷ Maybe NumericDateTimeFormat
      , fractionalSecondDigit ∷ Maybe Int
      , timeZoneName ∷ Maybe TimeZoneNameFormat
      )
  options = Option.recordToRecord options'

---- Static methods ------------------------------------------------------------
foreign import supportedLocalesOfImpl ∷ EffectFn2 (Array String) Foreign (Array String)

type SupportedLocalesOfOptions
  = ( localeMatcher ∷ LocaleMatcher
    )

-- | Returns an array containing those of the provided locales that are
-- | supported without having to fall back to the runtime’s default locale.
-- | ```purescript
-- |
-- | ```
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

  options ∷ Record ( localeMatcher ∷ Maybe LocaleMatcher )
  options = Option.recordToRecord options'

---- Instance methods ----------------------------------------------------------
foreign import formatImpl ∷ Fn2 JSDate DateTimeFormat String

-- | Getter function that formats a date according to the locale and formatting
-- | options of this `DateTimeFormat` object. This is treated as a pure
-- | function.
format ∷ JSDate → DateTimeFormat → String
format = runFn2 formatImpl

formatDateTime ∷ DateTime → DateTimeFormat → String
formatDateTime dateTime = format (JSDate.fromDateTime dateTime)

foreign import formatToPartsImpl ∷ Fn2 JSDate DateTimeFormat Foreign

formatToParts ∷ JSDate → DateTimeFormat → Array FormattedDateParts
formatToParts date =
  runFn2 formatToPartsImpl date
    >>> JSON.read
    >>> either (NEL.toUnfoldable >>> map renderForeignError >>> joinWith ", " >>> unsafeCrashWith) identity

-- resolvedOptionsImpl
-- resolvedOptions
foreign import formatRangeImpl ∷ Fn3 JSDate JSDate DateTimeFormat String

--
-- | This method receives two `JSDate`s and formats the date range in the most
-- | concise way based on the locale and options provided when instantiating
-- | `DateTimeFormat`.
formatRange ∷ { start ∷ JSDate, end ∷ JSDate } → DateTimeFormat → String
formatRange { start, end } = runFn3 formatRangeImpl start end

-- TODO: lacking browsers support + needs a concept for FormattedDateRangeParts
--foreign import formatRangeToPartsImpl ∷ Fn3 JSDate JSDate DateTimeFormat Foreign
-- | This method receives two `JSDate`s and returns an `Array` of `Record`s
-- | containing the locale-specific tokens representing each part of the
-- | formatted date range.
--formatRangeToParts ∷ JSDate → JSDate → DateTimeFormat → Array FormattedDateParts
--formatRangeToParts startDate endDate =
--  runFn3 formatRangeToPartsImpl startDate endDate
--    >>> JSON.read
--    >>> either (NEL.toUnfoldable >>> map renderForeignError >>> joinWith ", " >>> unsafeCrashWith) identity
