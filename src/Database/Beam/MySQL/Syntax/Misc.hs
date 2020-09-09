{-# LANGUAGE TypeFamilies #-}

module Database.Beam.MySQL.Syntax.Misc where

import           Data.Text (Text)
import           Database.Beam.Backend.SQL (IsSql92AggregationExpressionSyntax (..),
                                            IsSql92AggregationSetQuantifierSyntax (..),
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

data AggOp =
  Count |
  Avg |
  Sum |
  Min |
  Max
  deriving stock (Eq, Show)

data MySQLAggregationExpressionSyntax =
  CountAll |
  Aggregation
    !AggOp
    !(Maybe MySQLAggregationSetQuantifierSyntax)
    MySQLAggregationExpressionSyntax
  deriving stock (Eq, Show)

instance IsSql92AggregationExpressionSyntax MySQLAggregationExpressionSyntax where
  type Sql92AggregationSetQuantifierSyntax MySQLAggregationExpressionSyntax =
    MySQLAggregationSetQuantifierSyntax
  {-# INLINABLE countAllE #-}
  countAllE = CountAll
  {-# INLINABLE countE #-}
  countE = Aggregation Count
  {-# INLINABLE avgE #-}
  avgE = Aggregation Avg
  {-# INLINABLE sumE #-}
  sumE = Aggregation Sum
  {-# INLINABLE minE #-}
  minE = Aggregation Min
  {-# INLINABLE maxE #-}
  maxE = Aggregation Max
