-- Due to RDP plugin
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}
{-# LANGUAGE TypeFamilies #-}

module Database.Beam.MySQL.Syntax.Misc where

import           Data.Text (Text)
import           Database.Beam.Backend.SQL (IsSql92AggregationSetQuantifierSyntax (..),
                                            IsSql92ExtractFieldSyntax (..),
                                            IsSql92FieldNameSyntax (..),
                                            IsSql92QuantifierSyntax (..))

data MySQLFieldNameSyntax =
  QualifiedField {
    table :: {-# UNPACK #-} !Text,
    field :: {-# UNPACK #-} !Text
    } |
  UnqualifiedField {
    field :: {-# UNPACK #-} !Text
    }
  deriving stock (Eq, Show)

instance IsSql92FieldNameSyntax MySQLFieldNameSyntax where
  {-# INLINABLE qualifiedField #-}
  qualifiedField = QualifiedField
  {-# INLINABLE unqualifiedField #-}
  unqualifiedField = UnqualifiedField

data MySQLQuantifierSyntax = All | Any
 deriving stock (Eq, Show)

instance IsSql92QuantifierSyntax MySQLQuantifierSyntax where
  {-# INLINABLE quantifyOverAll #-}
  quantifyOverAll = All
  {-# INLINABLE quantifyOverAny #-}
  quantifyOverAny = Any

data MySQLExtractFieldSyntax =
  SecondsField |
  MinutesField |
  HourField |
  DayField |
  MonthField |
  YearField
  deriving stock (Eq, Show)

instance IsSql92ExtractFieldSyntax MySQLExtractFieldSyntax where
  {-# INLINABLE secondsField #-}
  secondsField = SecondsField
  {-# INLINABLE minutesField #-}
  minutesField = MinutesField
  {-# INLINABLE hourField #-}
  hourField = HourField
  {-# INLINABLE dayField #-}
  dayField = DayField
  {-# INLINABLE monthField #-}
  monthField = MonthField
  {-# INLINABLE yearField #-}
  yearField = YearField

data MySQLAggregationSetQuantifierSyntax =
  SetDistinct |
  SetAll
  deriving stock (Eq, Show)

instance IsSql92AggregationSetQuantifierSyntax MySQLAggregationSetQuantifierSyntax where
  {-# INLINABLE setQuantifierDistinct #-}
  setQuantifierDistinct = SetDistinct
  {-# INLINABLE setQuantifierAll #-}
  setQuantifierAll = SetAll
