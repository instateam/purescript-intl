module Intl.Common.MonthDateTimeFormat where

import Prelude
import Control.Alt ((<|>))
import Data.Either (Either(..), either)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Foreign (F, Foreign, ForeignError(..), fail, readString)
import Intl.Common.NumericDateTimeFormat (NumericDateTimeFormat(..))
import Intl.Common.NumericDateTimeFormat as NumericDateTimeFormat
import Intl.Common.StringDateTimeFormat (StringDateTimeFormat(..))
import Intl.Common.StringDateTimeFormat as StringDateTimeFormat
import Intl.RelativeTimeFormat.NumericFormat (NumericFormat)
import Simple.JSON (class ReadForeign, class WriteForeign, readImpl)
import Simple.JSON as JSON

data MonthDateTimeFormat
  = NumericFormat NumericDateTimeFormat
  | StringFormat StringDateTimeFormat

derive instance eqMonthDateTimeFormat ∷ Eq MonthDateTimeFormat

derive instance genericHourCycle ∷ Generic MonthDateTimeFormat _

instance showMonthDateTimeFormat ∷ Show MonthDateTimeFormat where
  show = genericShow

instance readForeignMonthDateTimeFormat ∷ JSON.ReadForeign MonthDateTimeFormat where
  readImpl frgn =
    (NumericFormat <$> JSON.read' frgn)
      <|> (StringFormat <$> JSON.read' frgn)

instance writeForeignDateTimeFormat ∷ JSON.WriteForeign MonthDateTimeFormat where
  writeImpl = case _ of
    NumericFormat ndtf → JSON.write ndtf
    StringFormat sdtf → JSON.write sdtf

toEither ∷ MonthDateTimeFormat → Either NumericDateTimeFormat StringDateTimeFormat
toEither = case _ of
  NumericFormat ndtf → Left ndtf
  StringFormat sdtf → Right sdtf

class
  (Show a, JSON.ReadForeign a, JSON.WriteForeign a) ⇐ MonthDateTimeFormatable a where
  from ∷ a → MonthDateTimeFormat
  to ∷ MonthDateTimeFormat → Maybe a

instance monthDateTimeFormatNumeric ∷ MonthDateTimeFormatable NumericDateTimeFormat where
  from = NumericFormat
  to = case _ of
    NumericFormat ndtf → Just ndtf
    StringFormat _ → Nothing

instance monthDateTimeFormatString ∷ MonthDateTimeFormatable StringDateTimeFormat where
  from = StringFormat
  to = case _ of
    StringFormat sdtf → Just sdtf
    NumericFormat _ → Nothing
