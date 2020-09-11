-- Due to RDP plugin
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Database.Beam.MySQL.Syntax.DataType where

import           Data.Bits (toIntegralSized, (.&.))
import           Data.ByteString.Lazy (ByteString)
import           Data.Text (Text)
import           Data.Word (Word16, Word8)
import           Database.Beam.Backend.SQL (IsSql92DataTypeSyntax (..))
import           Database.Beam.MySQL.Syntax.Render (RenderError (..),
                                                    Renderable (renderPass),
                                                    contextFreeRender)
import           Fmt ((+|), (|+))
import           Prelude hiding (length)

data MySQLPrecision =
  None |
  DigitsOnly {-# UNPACK #-} !Word |
  DigitsScale {-# UNPACK #-} !Word {-# UNPACK #-} !Word
  deriving stock (Eq, Show)

instance Renderable MySQLPrecision where
  {-# INLINABLE renderPass #-}
  renderPass = contextFreeRender go
    where
      go :: MySQLPrecision -> ByteString
      go = \case
        None -> ""
        DigitsOnly d -> "(" +| d |+ ")"
        DigitsScale d s -> "(" +| d |+ ", " +| s |+ ")"

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

instance Renderable MySQLDataTypeSyntax where
  {-# INLINABLE renderPass #-}
  renderPass = \case
    DomainType t -> contextFreeRender (\t' -> "`" +| t' |+ "`") t
    dts@CharType{} -> do
      len <- resolveCharLength dts.length
      contextFreeRender go len
      where
        go :: Text -> ByteString
        go len =
          "CHAR" +|
          len |+
          maybe " " (\cs -> " CHARACTER SET `" +|
                            cs |+
                            "`") dts.characterSet
    dts@VarCharType{} -> do
      len <- resolveVarCharLength dts.length
      contextFreeRender go len
      where
        go :: Text -> ByteString
        go len =
          "VARCHAR" +|
          len |+
          maybe " " (\cs -> " CHARACTER SET `" +|
                            cs |+
                            "`") dts.characterSet
    NationalCharType len -> do
      len' <- resolveCharLength len
      contextFreeRender go len'
      where
        go :: Text -> ByteString
        go len' =
          "NATIONAL CHAR" +| len' |+ " "
    NationalVarCharType len -> do
      len' <- resolveVarCharLength len
      contextFreeRender go len'
      where
        go :: Text -> ByteString
        go len' =
          "NATIONAL CHAR VARYING" +| len' |+ " "
    BitType len -> do
      len' <- resolveBitLength len
      contextFreeRender go len'
      where
        go :: Text -> ByteString
        go len' =
          "BIT" +| len' |+ " "
    VarBitType len -> do
      len' <- resolveVarBitLength len
      contextFreeRender go len'
      where
        go :: Text -> ByteString
        go len' =
          "VARBINARY" +| len' |+ " "

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

-- Helpers

resolveCharLength :: Maybe Word -> Either RenderError Text
resolveCharLength = \case
  Nothing -> Right $ "(" +| (maxBound @Word8) |+ ")"
  Just len -> case toIntegralSized len of
    Nothing              -> Left . CharLengthTooLarge $ len
    Just (len' :: Word8) -> Right $ "(" +| len' |+ ")"

-- This is not a perfect test, as it depends on the character set in use.
-- However, it at least ensures that the size makes _some_ sense.
resolveVarCharLength :: Maybe Word -> Either RenderError Text
resolveVarCharLength = \case
  Nothing -> Right $ "(" +| (maxBound @Word16) |+ ")"
  Just len -> case toIntegralSized len of
    Nothing               -> Left . VarCharLengthTooLarge $ len
    Just (len' :: Word16) -> Right $ "(" +| len' |+ ")"

resolveBitLength :: Maybe Word -> Either RenderError Text
resolveBitLength = \case
  Nothing -> Right "(64)"
  Just 0 -> Left . InvalidBitLength $ 0
  Just len -> if len < 65
              then Right $ "(" +| len |+ ")"
              else Left . InvalidBitLength $ len

resolveVarBitLength :: Maybe Word -> Either RenderError Text
resolveVarBitLength = \case
  Nothing -> Right $ "(" +| (maxBound @Word16) |+ ")"
  -- 2^16 - 1 bytes is the limit for VARBINARY
  -- That's 2^19 - 8 bits
  Just len -> if len <= 524280
              -- Next highest multiple of 8, then divide by 8 to get bytes
              then Right $ "(" +| (((len + 7) .&. negate 8) `div` 8) |+ ")"
              else Left . InvalidVarBitLength $ len
