module Database.Beam.MySQL.Syntax.DataType where

import           Data.Text (Text)
import           Database.Beam.Backend.SQL (IsSql92DataTypeSyntax (..))

data MySQLPrecision =
  None |
  Lower {-# UNPACK #-} !Word |
  Both {-# UNPACK #-} !Word {-# UNPACK #-} !Word
  deriving stock (Eq, Show)

intoMySQLPrecision :: Maybe (Word, Maybe Word) -> MySQLPrecision
intoMySQLPrecision = \case
  Nothing -> None
  Just (d, mn) -> case mn of
    Nothing -> Lower d
    Just n  -> Both d n

data MySQLDataTypeSyntax =
  DomainType {-# UNPACK #-} !Text |
  CharType !(Maybe Word) !(Maybe Text) | -- length, character set
  VarCharType !(Maybe Word) !(Maybe Text) | -- length, character set
  NationalCharType !(Maybe Word) | -- length
  NationalVarCharType !(Maybe Word) | -- length
  BitType !(Maybe Word) | -- length
  VarBitType !(Maybe Word) | -- length
  NumericType !MySQLPrecision |
  DecimalType !MySQLPrecision |
  IntType |
  SmallIntType |
  FloatType !(Maybe Word) | -- precision
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


