module Intl.Common.FormattedDateParts where

import Prelude
import Data.Symbol (SProxy(..))
import Data.Variant (Variant)
import Data.Variant as V

type FormattedDateParts
  = Variant
      ( day ∷ String
      , dayPeriod ∷ String
      , era ∷ String
      , fractionalSecond ∷ String
      , hour ∷ String
      , literal ∷ String
      , minute ∷ String
      , month ∷ String
      , relatedYear ∷ String
      , second ∷ String
      , timeZoneName ∷ String
      , weekday ∷ String
      , year ∷ String
      , yearName ∷ String
      )

_day = SProxy ∷ SProxy "day"

_dayPeriod = SProxy ∷ SProxy "dayPeriod"

_era = SProxy ∷ SProxy "era"

_fractionalSecond = SProxy ∷ SProxy "fractionalSecond"

_hour = SProxy ∷ SProxy "hour"

_literal = SProxy ∷ SProxy "literal"

_minute = SProxy ∷ SProxy "minute"

_month = SProxy ∷ SProxy "month"

_relatedYear = SProxy ∷ SProxy "relatedYear"

_second = SProxy ∷ SProxy "second"

_timeZoneName = SProxy ∷ SProxy "timeZoneName"

_weekday = SProxy ∷ SProxy "weekday"

_year = SProxy ∷ SProxy "year"

_yearName = SProxy ∷ SProxy "yearName"
