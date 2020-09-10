-- Due to RDP plugin
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

module Database.Beam.MySQL.Syntax.DataType where

import           Data.Text (Text)
import           Database.Beam.Backend.SQL (IsSql92DataTypeSyntax (..))
import           Prelude hiding (length)

data MySQLPrecision =
  None |
  DigitsOnly {-# UNPACK #-} !Word |
  DigitsScale {-# UNPACK #-} !Word {-# UNPACK #-} !Word
  deriving stock (Eq, Show)

intoMySQLPrecision :: Maybe (Word, Maybe Word) -> MySQLPrecision
intoMySQLPrecision = \case
  Nothing -> None
  Just (d, mn) -> case mn of
    Nothing -> DigitsOnly d
    Just n  -> DigitsScale d n

data MySQLDataTypeSyntax =
  DomainType {-# UNPACK #-} !Text |
  CharType {
    length       :: !(Maybe Word),
    characterSet :: !(Maybe Text)
    } |
  VarCharType {
    length       :: !(Maybe Word),
    characterSet :: !(Maybe Text)
    } |
  NationalCharType {
    length :: !(Maybe Word)
    } |
  NationalVarCharType {
    length :: !(Maybe Word)
    } |
  BitType {
    length :: !(Maybe Word)
    } |
  VarBitType {
    length :: !(Maybe Word)
    } |
  NumericType !MySQLPrecision |
  DecimalType !MySQLPrecision |
  IntType |
  SmallIntType |
  FloatType {
    precision :: !(Maybe Word)
    } |
  DoubleType |
  RealType |
  DateType |
  TimeType |
  TimestampType
  deriving stock (Eq, Show)

instance IsSql92DataTypeSyntax MySQLDataTypeSyntax where
  {-# INLINABLE domainType #-}
  domainType = DomainType
  {-# INLINABLE charType #-}
  charType = CharType
  {-# INLINABLE varCharType #-}
  varCharType = VarCharType
  {-# INLINABLE nationalCharType #-}
  nationalCharType = NationalCharType
  {-# INLINABLE nationalVarCharType #-}
  nationalVarCharType = NationalVarCharType
  {-# INLINABLE bitType #-}
  bitType = BitType
  {-# INLINABLE varBitType #-}
  varBitType = VarBitType
  {-# INLINABLE numericType #-}
  numericType = NumericType . intoMySQLPrecision
  {-# INLINABLE decimalType #-}
  decimalType = DecimalType . intoMySQLPrecision
  {-# INLINABLE intType #-}
  intType = IntType
  {-# INLINABLE smallIntType #-}
  smallIntType = SmallIntType
  {-# INLINABLE floatType #-}
  floatType = FloatType
  {-# INLINABLE doubleType #-}
  doubleType = DoubleType
  {-# INLINABLE realType #-}
  realType = RealType
  {-# INLINABLE dateType #-}
  dateType = DateType
  {-# INLINABLE timeType #-}
  timeType _ _ = TimeType
  {-# INLINABLE timestampType #-}
  timestampType _ _ = TimestampType


