-- https://tc39.es/ecma402/#sec-properties-of-intl-relativetimeformat-prototype-object
module Intl.RelativeTimeFormat
  ( RelativeTimeFormat
  , create
  , RelativeTimeFormatOptions
  , supportedLocalesOf
  , SupportedLocalesOfOptions
  , format
  , formatToParts
  , resolvedOptions
  , module Intl.Common.StringDateTimeFormat
  , module Intl.Common.LocaleMatcher
  , module Intl.Common.LocaleTag
  , module Intl.Common.NumericOutput
  , module Intl.Common.TimeUnit
  ) where

import Prelude
import Data.DateTime (DateTime(..))
import Data.Either (either)
import Data.Function.Uncurried (Fn2, Fn3, runFn2, runFn3)
import Data.List.NonEmpty (toUnfoldable) as NEL
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap, wrap)
import Data.String (joinWith)
import Data.Time (Time)
import Data.Time as Time
import Effect (Effect)
import Effect as Effect
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn4, runEffectFn1, runEffectFn2, runEffectFn4)
import Foreign (Foreign, renderForeignError)
import Intl.Common.FormattedDateParts (FormattedDateParts)
import Intl.Common.LocaleMatcher (LocaleMatcher(..))
import Intl.Common.LocaleTag (LocaleTag(..), localesToForeign)
import Intl.Common.LocaleTag as LocaleTag
import Intl.Common.NumericOutput (NumericOutput(..))
import Intl.Common.StringDateTimeFormat (StringDateTimeFormat(..))
import Intl.Common.TimeUnit (TimeUnit(..))
import Option as Option
import Partial.Unsafe (unsafeCrashWith)
import Simple.JSON as JSON

foreign import data RelativeTimeFormat ∷ Type

---- Constructor ---------------------------------------------------------------
type RelativeTimeFormatOptions
  = ( localeMatcher ∷ LocaleMatcher
    , numeric ∷ NumericOutput
    , style ∷ StringDateTimeFormat
    )

foreign import createImpl ∷ EffectFn2 Foreign Foreign RelativeTimeFormat

-- | Creates a new `Intl.RelativeTimeFormat` formatter with `Array` of locales
-- | (falling back on subsequent locales) and a `Record` of optional options.
-- |
-- | ```purescript
-- | import Intl.Common.StringDateTimeFormat as StringDateTimeFormat
-- | import Intl.Common.LocaleMatcher as LocaleMatcher
-- | import Intl.Common.LocaleTag (LocaleTag(..))
-- | import Intl.Common.NumericOutput as NumericOutput
-- | import Intl.RelativeTimeFormat (RelativeTimeFormat)
-- | import Intl.RelativeTimeFormat as RelativeTimeFormat
-- |
-- | formatter ∷ Effect RelativeTimeFormat
-- | formatter =
-- |   RelativeTimeFormat.create
-- |     [ LocaleTag "en-CA", LocaleTag "en-US", LocaleTag "en" ]
-- |     { localeMatcher: LocaleMatcher.BestFit
-- |     , numeric: NumericOutput.Always
-- |     , style: StringDateTimeFormat.Narrow
-- |     }
-- | ```
create ∷
  ∀ record.
  Option.FromRecord record () RelativeTimeFormatOptions ⇒
  Array LocaleTag →
  Record record →
  Effect RelativeTimeFormat
create localeTags options'' = runEffectFn2 createImpl locales (JSON.write options)
  where
  locales ∷ Foreign
  locales = LocaleTag.localesToForeign localeTags

  options' ∷ Option.Record () RelativeTimeFormatOptions
  options' = Option.recordFromRecord options''

  options ∷
    Record
      ( localeMatcher ∷ Maybe LocaleMatcher
      , numeric ∷ Maybe NumericOutput
      , style ∷ Maybe StringDateTimeFormat
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

  options ∷ Record ( localeMatcher ∷ Maybe LocaleMatcher )
  options = Option.recordToRecord options'

---- Instance methods ----------------------------------------------------------
foreign import formatImpl ∷ Fn3 Number Foreign RelativeTimeFormat String

-- | Formats a value and a unit according to the locale and formatting options
-- | of the given `Intl.RelativeTimeFormat` object. This is treated as a pure
-- | function.
-- | ```purescript
-- | import Intl.Common.StringDateTimeFormat as StringDateTimeFormat
-- | import Intl.Common.LocaleTag (LocaleTag(..))
-- | import.Intl.Common.TimeUnit as TimeUnit
-- | import Intl.RelativeTimeFormat (create, format)
-- | import Test (assert)
-- |
-- | main ∷ Effect Unit
-- | main = do
-- |   formatter ← create [ LocaleTag "en-US" ] { style: StringDateTimeFormat.Narrow }
-- |   assert $ format (-5.0) TimeUnit.Day formatter == "5 days ago"
-- |   assert $ format 0.0 TimeUnit.Second formatter == "now"
-- | ```
format ∷ Number → TimeUnit → RelativeTimeFormat → String
format value unit = runFn3 formatImpl value (JSON.write unit)

-- TODO: wrong output
foreign import formatToPartsImpl ∷ Fn3 Number Foreign RelativeTimeFormat Foreign

-- | Returns an `Array` of objects representing the relative time format in
-- | parts that can be used for custom locale-aware formatting.
formatToParts ∷ Number → TimeUnit → RelativeTimeFormat → Array FormattedDateParts
formatToParts value timeUnit =
  runFn3 formatToPartsImpl value (JSON.write timeUnit)
    >>> JSON.read
    >>> either (NEL.toUnfoldable >>> map renderForeignError >>> joinWith ", " >>> unsafeCrashWith) identity

foreign import resolvedOptionsImpl ∷ EffectFn1 RelativeTimeFormat Foreign

type ResolvedOptions
  = { locale ∷ LocaleTag
    , style ∷ StringDateTimeFormat
    , numeric ∷ NumericOutput
    , numberingSystem ∷ String -- TODO
    }

-- | Returns a new object with properties reflecting the locale and formatting
-- | options computed during initialization of the object
resolvedOptions ∷ RelativeTimeFormat → Effect (JSON.E ResolvedOptions)
resolvedOptions formatter = do
  ropts ← runEffectFn1 resolvedOptionsImpl formatter
  pure (JSON.read ropts)
