{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TypeApplications       #-}

module Database.Beam.MySQL.FromField where

import           Data.Bits (Bits (zeroBits), toIntegralSized)
import           Data.ByteString (ByteString)
import           Data.Int (Int16, Int32, Int64, Int8)
import           Data.Kind (Type)
import           Data.Scientific (Scientific, toBoundedInteger)
import           Data.Text (Text, unpack)
import           Data.Text.Encoding (encodeUtf8)
import           Data.Word (Word16, Word32, Word64, Word8)
import           Database.Beam.Backend.SQL (SqlNull (SqlNull))
import           Database.MySQL.Base (MySQLValue (..))
import           Text.Read (readMaybe)

data Leniency = Lenient | Strict
  deriving stock (Eq, Show)

data FromFieldResult (l :: Leniency) (a :: Type) where
  UnexpectedNull :: FromFieldResult l a
  TypeMismatch :: FromFieldResult l a
  Won'tFit :: FromFieldResult l a
  IEEENaN :: FromFieldResult 'Lenient a
  IEEEInfinity :: FromFieldResult 'Lenient a
  IEEETooSmall :: FromFieldResult 'Lenient a
  IEEETooBig :: FromFieldResult 'Lenient a
  TextCouldNotParse :: FromFieldResult 'Lenient a
  LenientParse :: a -> FromFieldResult 'Lenient a
  StrictParse :: a -> FromFieldResult l a

deriving stock instance (Eq a) => Eq (FromFieldResult l a)

deriving stock instance (Show a) => Show (FromFieldResult l a)

deriving stock instance Functor (FromFieldResult l)

newtype L (a :: Type) = L a
  deriving newtype (Eq)
  deriving stock (Show)

class FromField (l :: Leniency) (a :: Type) | a -> l where
  fromField :: MySQLValue -> FromFieldResult l a

instance FromField 'Strict Bool where
  {-# INLINABLE fromField #-}
  fromField = \case
    MySQLInt8 v -> StrictParse (zeroBits /= v)
    MySQLInt8U v -> StrictParse (zeroBits /= v)
    MySQLBit v -> StrictParse (zeroBits /= v)
    MySQLNull -> UnexpectedNull
    _ -> TypeMismatch

instance FromField 'Lenient (L Bool) where
  {-# INLINABLE fromField #-}
  fromField = relax . fmap L . fromField

instance FromField 'Strict Int8 where
  {-# INLINABLE fromField #-}
  fromField = \case
    MySQLInt8 v -> StrictParse v
    MySQLDecimal v -> tryScientific v
    MySQLNull -> UnexpectedNull
    _ -> TypeMismatch

instance FromField 'Lenient (L Int8) where
  {-# INLINABLE fromField #-}
  fromField v = fmap L $ case fromField v of
    TypeMismatch -> case v of
      MySQLText v'   -> tryText v'
      MySQLDouble v' -> tryIEEE v'
      MySQLFloat v'  -> tryIEEE v'
      _              -> TypeMismatch
    res          -> relax res

instance FromField 'Strict Int16 where
  {-# INLINABLE fromField #-}
  fromField = \case
    MySQLInt8 v -> StrictParse . fromIntegral $ v
    MySQLInt16 v -> StrictParse v
    MySQLDecimal v -> tryScientific v
    MySQLNull -> UnexpectedNull
    _ -> TypeMismatch

instance FromField 'Lenient (L Int16) where
  {-# INLINABLE fromField #-}
  fromField v = fmap L $ case fromField v of
    TypeMismatch -> case v of
      MySQLText v'   -> tryText v'
      MySQLDouble v' -> tryIEEE v'
      MySQLFloat v'  -> tryIEEE v'
      _              -> TypeMismatch
    res -> relax res

instance FromField 'Strict Int32 where
  {-# INLINABLE fromField #-}
  fromField = \case
    MySQLInt8 v -> StrictParse . fromIntegral $ v
    MySQLInt16 v -> StrictParse . fromIntegral $ v
    MySQLInt32 v -> StrictParse v
    MySQLDecimal v -> tryScientific v
    MySQLNull -> UnexpectedNull
    _ -> TypeMismatch

instance FromField 'Lenient (L Int32) where
  {-# INLINABLE fromField #-}
  fromField v = fmap L $ case fromField v of
    TypeMismatch -> case v of
      MySQLText v'   -> tryText v'
      MySQLDouble v' -> tryIEEE v'
      MySQLFloat v'  -> tryIEEE v'
      _              -> TypeMismatch
    res -> relax res

instance FromField 'Strict Int64 where
  {-# INLINABLE fromField #-}
  fromField = \case
    MySQLInt8 v -> StrictParse . fromIntegral $ v
    MySQLInt16 v -> StrictParse . fromIntegral $ v
    MySQLInt32 v -> StrictParse . fromIntegral $ v
    MySQLInt64 v -> StrictParse v
    MySQLDecimal v -> tryScientific v
    MySQLNull -> UnexpectedNull
    _ -> TypeMismatch

instance FromField 'Lenient (L Int64) where
  {-# INLINABLE fromField #-}
  fromField v = fmap L $ case fromField v of
    TypeMismatch -> case v of
      MySQLText v'   -> tryText v'
      MySQLDouble v' -> tryIEEE v'
      MySQLFloat v'  -> tryIEEE v'
      _              -> TypeMismatch
    res -> relax res

instance FromField 'Strict Int where
  {-# INLINABLE fromField #-}
  fromField = \case
    MySQLInt8 v -> StrictParse . fromIntegral $ v
    MySQLInt16 v -> StrictParse . fromIntegral $ v
    MySQLInt32 v -> tryFixed v
    MySQLInt64 v -> tryFixed v
    MySQLDecimal v -> tryScientific v
    MySQLNull -> UnexpectedNull
    _ -> TypeMismatch

instance FromField 'Lenient (L Int) where
  {-# INLINABLE fromField #-}
  fromField v = fmap L $ case fromField v of
    TypeMismatch -> case v of
      MySQLText v'   -> tryText v'
      MySQLDouble v' -> tryIEEE v'
      MySQLFloat v'  -> tryIEEE v'
      _              -> TypeMismatch
    res -> relax res

instance FromField 'Strict Word8 where
  {-# INLINABLE fromField #-}
  fromField = \case
    MySQLInt8U v -> StrictParse v
    MySQLDecimal v -> tryScientific v
    MySQLNull -> UnexpectedNull
    _ -> TypeMismatch

instance FromField 'Lenient (L Word8) where
  {-# INLINABLE fromField #-}
  fromField = fmap L . relax . fromField

instance FromField 'Strict Word16 where
  {-# INLINABLE fromField #-}
  fromField = \case
    MySQLInt8U v -> StrictParse . fromIntegral $ v
    MySQLInt16U v -> StrictParse v
    MySQLDecimal v -> tryScientific v
    MySQLNull -> UnexpectedNull
    _ -> TypeMismatch

instance FromField 'Lenient (L Word16) where
  {-# INLINABLE fromField #-}
  fromField = fmap L . relax . fromField

instance FromField 'Strict Word32 where
  {-# INLINABLE fromField #-}
  fromField = \case
    MySQLInt8U v -> StrictParse . fromIntegral $ v
    MySQLInt16U v -> StrictParse . fromIntegral $ v
    MySQLInt32U v -> StrictParse v
    MySQLDecimal v -> tryScientific v
    MySQLNull -> UnexpectedNull
    _ -> TypeMismatch

instance FromField 'Lenient (L Word32) where
  {-# INLINABLE fromField #-}
  fromField = fmap L . relax . fromField

instance FromField 'Strict Word64 where
  {-# INLINABLE fromField #-}
  fromField = \case
    MySQLInt8U v -> StrictParse . fromIntegral $ v
    MySQLInt16U v -> StrictParse . fromIntegral $ v
    MySQLInt32U v -> StrictParse . fromIntegral $ v
    MySQLInt64U v -> StrictParse v
    MySQLDecimal v -> tryScientific v
    MySQLNull -> UnexpectedNull
    _ -> TypeMismatch

instance FromField 'Lenient (L Word64) where
  {-# INLINABLE fromField #-}
  fromField = fmap L . relax . fromField

instance FromField 'Strict Word where
  {-# INLINABLE fromField #-}
  fromField = \case
    MySQLInt8U v -> StrictParse . fromIntegral $ v
    MySQLInt16U v -> StrictParse . fromIntegral $ v
    MySQLInt32U v -> tryFixed v
    MySQLInt64U v -> tryFixed v
    MySQLDecimal v -> tryScientific v
    MySQLNull -> UnexpectedNull
    _ -> TypeMismatch

instance FromField 'Lenient (L Word) where
  {-# INLINABLE fromField #-}
  fromField = fmap L . relax . fromField

instance FromField 'Strict Float where
  {-# INLINABLE fromField #-}
  fromField = \case
    MySQLFloat v -> StrictParse v
    MySQLNull -> UnexpectedNull
    _ -> TypeMismatch

instance FromField 'Strict Double where
  {-# INLINABLE fromField #-}
  fromField = \case
    MySQLFloat v -> StrictParse . realToFrac $ v
    MySQLDouble v -> StrictParse v
    MySQLNull -> UnexpectedNull
    _ -> TypeMismatch

-- TODO: Lenient versions for these. - Koz

instance FromField 'Strict Scientific where
  {-# INLINABLE fromField #-}
  fromField = \case
    MySQLDecimal v -> StrictParse v
    MySQLInt8 v -> StrictParse . fromIntegral $ v
    MySQLInt8U v -> StrictParse . fromIntegral $ v
    MySQLInt16 v -> StrictParse . fromIntegral $ v
    MySQLInt16U v -> StrictParse . fromIntegral $ v
    MySQLInt32 v -> StrictParse . fromIntegral $ v
    MySQLInt32U v -> StrictParse . fromIntegral $ v
    MySQLInt64 v -> StrictParse . fromIntegral $ v
    MySQLInt64U v -> StrictParse . fromIntegral $ v
    MySQLNull -> UnexpectedNull
    _ -> TypeMismatch

instance FromField 'Lenient (L Scientific) where
  {-# INLINABLE fromField #-}
  fromField = fmap L . relax . fromField

instance FromField 'Strict Rational where
  {-# INLINABLE fromField #-}
  fromField = \case
    MySQLInt8 v -> StrictParse . fromIntegral $ v
    MySQLInt8U v -> StrictParse . fromIntegral $ v
    MySQLInt16 v -> StrictParse . fromIntegral $ v
    MySQLInt16U v -> StrictParse . fromIntegral $ v
    MySQLInt32 v -> StrictParse . fromIntegral $ v
    MySQLInt32U v -> StrictParse . fromIntegral $ v
    MySQLInt64 v -> StrictParse . fromIntegral $ v
    MySQLInt64U v -> StrictParse . fromIntegral $ v
    MySQLNull -> UnexpectedNull
    _ -> TypeMismatch

instance FromField 'Lenient (L Rational) where
  {-# INLINABLE fromField #-}
  fromField = fmap L . relax . fromField

instance FromField 'Strict SqlNull where
  fromField = \case
    MySQLNull -> StrictParse SqlNull
    _ -> TypeMismatch

instance FromField 'Lenient (L SqlNull) where
  fromField = fmap L . relax . fromField

instance FromField 'Strict ByteString where
  fromField = \case
    MySQLText v -> StrictParse . encodeUtf8 $ v
    MySQLBytes v -> StrictParse v
    MySQLNull -> UnexpectedNull
    _ -> TypeMismatch

instance FromField 'Lenient (L ByteString) where
  fromField = fmap L . relax . fromField

instance FromField 'Strict Text where
  fromField = \case
    MySQLText v -> StrictParse v
    MySQLNull -> UnexpectedNull
    _ -> TypeMismatch

-- TODO: Lenient for Text. - Koz

-- Helpers

relax :: FromFieldResult 'Strict a -> FromFieldResult 'Lenient a
relax = \case
  UnexpectedNull -> UnexpectedNull
  TypeMismatch -> TypeMismatch
  Won'tFit -> Won'tFit
  StrictParse x -> StrictParse x

tryScientific :: (Integral a, Bounded a) =>
  Scientific -> FromFieldResult 'Strict a
tryScientific = maybe Won'tFit StrictParse . toBoundedInteger

tryText :: (Read a) => Text -> FromFieldResult 'Lenient a
tryText = maybe TextCouldNotParse LenientParse . readMaybe . unpack

tryIEEE :: forall (b :: Type) (a :: Type) .
  (RealFloat a, Integral b, Bounded b) =>
  a -> FromFieldResult 'Lenient b
tryIEEE v
  | isNaN v = IEEENaN
  | isInfinite v = IEEEInfinity
  | v < fromIntegral (minBound @b) = IEEETooSmall
  | v > fromIntegral (minBound @b) = IEEETooBig
  | otherwise = LenientParse . truncate $ v

tryFixed :: (Integral a, Integral b, Bits a, Bits b) =>
  a -> FromFieldResult 'Strict b
tryFixed = maybe Won'tFit StrictParse . toIntegralSized

{-



{-# LANGUAGE MultiWayIf #-}

module Database.Beam.MySQL.FromField (FromField (..)) where

import           Data.Aeson (Value, eitherDecodeStrict')
import           Data.Bits (finiteBitSize, zeroBits)
import           Data.ByteString (ByteString)
import           Data.Int (Int16, Int32, Int64, Int8)
import           Data.Scientific (Scientific, toBoundedInteger)
import           Data.Text (Text)
import           Data.Text.Encoding (encodeUtf8)
import           Data.Time.Calendar (Day)
import           Data.Time.Clock (NominalDiffTime)
import           Data.Time.LocalTime (LocalTime (..), TimeOfDay,
                                      daysAndTimeOfDayToTime, midnight)
import           Data.Word (Word16, Word32, Word64, Word8)
import           Database.Beam.Backend.SQL (SqlNull (..))
import           Database.Beam.Backend.SQL.Row (ColumnParseError (..))
import           Database.MySQL.Protocol.ColumnDef (FieldType, mySQLTypeBit,
                                                    mySQLTypeBlob,
                                                    mySQLTypeDate,
                                                    mySQLTypeDateTime,
                                                    mySQLTypeDateTime2,
                                                    mySQLTypeDecimal,
                                                    mySQLTypeDouble,
                                                    mySQLTypeEnum,
                                                    mySQLTypeFloat,
                                                    mySQLTypeInt24,
                                                    mySQLTypeLong,
                                                    mySQLTypeLongBlob,
                                                    mySQLTypeLongLong,
                                                    mySQLTypeMediumBlob,
                                                    mySQLTypeNewDate,
                                                    mySQLTypeNewDecimal,
                                                    mySQLTypeNull, mySQLTypeSet,
                                                    mySQLTypeShort,
                                                    mySQLTypeString,
                                                    mySQLTypeTime,
                                                    mySQLTypeTime2,
                                                    mySQLTypeTimestamp,
                                                    mySQLTypeTimestamp2,
                                                    mySQLTypeTiny,
                                                    mySQLTypeTinyBlob,
                                                    mySQLTypeVarChar,
                                                    mySQLTypeVarString,
                                                    mySQLTypeYear)
import           Database.MySQL.Protocol.MySQLValue (MySQLValue (..))
import           Fmt ((+|), (|+))

class FromField a where
  fromField :: FieldType -> MySQLValue -> Either ColumnParseError a

instance FromField Bool where
  fromField t = \case
    MySQLInt8 v  -> pure (zeroBits /= v)
    MySQLInt8U v -> pure (zeroBits /= v)
    MySQLBit v   -> pure (zeroBits /= v)
    MySQLNull    -> Left ColumnUnexpectedNull
    v            -> throwTypeMismatch "Bool" t v

instance FromField Int8 where
  fromField t = \case
    MySQLInt8 v    -> pure v
    MySQLDecimal v -> tryPackDecimal "Int8" v
    MySQLNull      -> Left ColumnUnexpectedNull
    v              -> throwTypeMismatch "Int8" t v

instance FromField Int16 where
  fromField t = \case
    MySQLInt8 v    -> pure . fromIntegral $ v
    MySQLInt16 v   -> pure v
    MySQLDecimal v -> tryPackDecimal "Int16" v
    MySQLNull      -> Left ColumnUnexpectedNull
    v              -> throwTypeMismatch "Int16" t v

instance FromField Int32 where
  fromField t = \case
    MySQLInt8 v    -> pure . fromIntegral $ v
    MySQLInt16 v   -> pure . fromIntegral $ v
    MySQLInt32 v   -> pure v
    MySQLDecimal v -> tryPackDecimal "Int32" v
    MySQLNull      -> Left ColumnUnexpectedNull
    v              -> throwTypeMismatch "Int32" t v

instance FromField Int64 where
  fromField t = \case
    MySQLInt8 v    -> pure . fromIntegral $ v
    MySQLInt16 v   -> pure . fromIntegral $ v
    MySQLInt32 v   -> pure . fromIntegral $ v
    MySQLInt64 v   -> pure v
    MySQLDecimal v -> tryPackDecimal "Int64" v
    MySQLNull      -> Left ColumnUnexpectedNull
    v              -> throwTypeMismatch "Int64" t v

instance FromField Int where
  fromField t = \case
    MySQLInt8 v    -> pure . fromIntegral $ v
    MySQLInt16 v   -> pure . fromIntegral $ v
    MySQLInt32 v   -> pure . fromIntegral $ v
    MySQLInt64 v   ->
      if finiteBitSize (zeroBits :: Int) == 64
        then pure . fromIntegral $ v
        else throwTypeMismatch "Int" t (MySQLInt64 v)
    MySQLDecimal v -> tryPackDecimal "Int" v
    MySQLNull      -> Left ColumnUnexpectedNull
    v              -> throwTypeMismatch "Int" t v

instance FromField Word8 where
  fromField t = \case
    MySQLInt8U v   -> pure v
    MySQLDecimal v -> tryPackDecimal "Word8" v
    MySQLNull      -> Left ColumnUnexpectedNull
    v              -> throwTypeMismatch "Word8" t v

instance FromField Word16 where
  fromField t = \case
    MySQLInt8U v   -> pure . fromIntegral $ v
    MySQLInt16U v  -> pure v
    MySQLDecimal v -> tryPackDecimal "Word16" v
    MySQLNull      -> Left ColumnUnexpectedNull
    v              -> throwTypeMismatch "Word16" t v

instance FromField Word32 where
  fromField t = \case
    MySQLInt8U v   -> pure . fromIntegral $ v
    MySQLInt16U v  -> pure . fromIntegral $ v
    MySQLInt32U v  -> pure v
    MySQLDecimal v -> tryPackDecimal "Word32" v
    MySQLNull      -> Left ColumnUnexpectedNull
    v              -> throwTypeMismatch "Word32" t v

instance FromField Word64 where
  fromField t = \case
    MySQLInt8U v   -> pure . fromIntegral $ v
    MySQLInt16U v  -> pure . fromIntegral $ v
    MySQLInt32U v  -> pure . fromIntegral $ v
    MySQLInt64U v  -> pure v
    MySQLDecimal v -> tryPackDecimal "Word64" v
    MySQLNull      -> Left ColumnUnexpectedNull
    v              -> throwTypeMismatch "Word64" t v

instance FromField Word where
  fromField t = \case
    MySQLInt8U v  -> pure . fromIntegral $ v
    MySQLInt16U v -> pure . fromIntegral $ v
    MySQLInt32U v -> pure . fromIntegral $ v
    MySQLInt64U v ->
      if finiteBitSize (zeroBits :: Word) == 64
      then pure . fromIntegral $ v
      else throwTypeMismatch "Word" t (MySQLInt64U v)
    MySQLNull     -> Left ColumnUnexpectedNull
    v             -> throwTypeMismatch "Word" t v

instance FromField Float where
  fromField t = \case
    MySQLFloat v -> pure v
    MySQLNull    -> Left ColumnUnexpectedNull
    v            -> throwTypeMismatch "Float" t v

instance FromField Double where
  fromField t = \case
    MySQLFloat v  -> pure . realToFrac $ v
    MySQLDouble v -> pure v
    MySQLNull     -> Left ColumnUnexpectedNull
    v             -> throwTypeMismatch "Double" t v

instance FromField Scientific where
  fromField t = \case
    MySQLDecimal v -> pure v
    MySQLInt8 v    -> pure . fromIntegral $ v
    MySQLInt8U v   -> pure . fromIntegral $ v
    MySQLInt16 v   -> pure . fromIntegral $ v
    MySQLInt16U v  -> pure . fromIntegral $ v
    MySQLInt32 v   -> pure . fromIntegral $ v
    MySQLInt32U v  -> pure . fromIntegral $ v
    MySQLInt64 v   -> pure . fromIntegral $ v
    MySQLInt64U v  -> pure . fromIntegral $ v
    MySQLNull      -> Left ColumnUnexpectedNull
    v              -> throwTypeMismatch "Scientific" t v

instance FromField Rational where
  fromField t = \case
    MySQLInt8 v   -> pure . fromIntegral $ v
    MySQLInt8U v  -> pure . fromIntegral $ v
    MySQLInt16 v  -> pure . fromIntegral $ v
    MySQLInt16U v -> pure . fromIntegral $ v
    MySQLInt32 v  -> pure . fromIntegral $ v
    MySQLInt32U v -> pure . fromIntegral $ v
    MySQLInt64 v  -> pure . fromIntegral $ v
    MySQLInt64U v -> pure . fromIntegral $ v
    MySQLNull     -> Left ColumnUnexpectedNull
    v             -> throwTypeMismatch "Rational" t v

instance FromField SqlNull where
  fromField t v = case v of
    MySQLNull -> pure SqlNull
    _         -> Left errorMsg
    where
      errorMsg :: ColumnParseError
      errorMsg = ColumnTypeMismatch "Present value" "NULL" msg
      msg :: String
      msg = "Unexpected actual value of type" <>
            fieldTypeToString t <>
            " with value " <>
            show v

instance FromField ByteString where
  fromField t = \case
    MySQLText v  -> pure . encodeUtf8 $ v
    MySQLBytes v -> pure v
    MySQLNull    -> Left ColumnUnexpectedNull
    v            -> throwTypeMismatch "ByteString" t v

instance FromField Text where
  fromField t = \case
    MySQLText v  -> pure v
    MySQLBytes v ->
      Left . ColumnTypeMismatch "Text" (fieldTypeToString t) . bytesErr $ v
    MySQLNull    -> Left ColumnUnexpectedNull
    v            -> throwTypeMismatch "Text" t v
    where
      bytesErr :: ByteString -> String
      bytesErr v = "Tried to read bytes as text: " +|
                   show v |+
                   ""

instance FromField LocalTime where
  fromField t = \case
    MySQLDateTime v  -> pure v
    MySQLTimeStamp v -> pure v
    MySQLDate v      -> pure (LocalTime v midnight)
    MySQLNull        -> Left ColumnUnexpectedNull
    v                -> throwTypeMismatch "LocalTime" t v

instance FromField Day where
  fromField t = \case
    MySQLDate v -> pure v
    MySQLNull   -> Left ColumnUnexpectedNull
    v           -> throwTypeMismatch "Day" t v

instance FromField TimeOfDay where
  fromField t = \case
    MySQLTime s v ->
      if s == zeroBits
        then pure v
        else throwTypeMismatch "TimeOfDay" t (MySQLTime s v)
    MySQLNull     -> Left ColumnUnexpectedNull
    v             -> throwTypeMismatch "TimeOfDay" t v

instance FromField NominalDiffTime where
  fromField t = \case
    MySQLTime s v -> do
      let isPositive = s == zeroBits
      let ndt = daysAndTimeOfDayToTime 0 v
      if isPositive
        then pure ndt
        else pure . negate $ ndt
    MySQLNull     -> Left ColumnUnexpectedNull
    v             -> throwTypeMismatch "NominalDiffTime" t v

instance FromField Value where
  fromField t = \case
    MySQLText v  -> case eitherDecodeStrict' . encodeUtf8 $ v of
      Left err  -> Left . ColumnErrorInternal $ "JSON parsing failed: " <> err
      Right val -> pure val
    MySQLBytes v -> case eitherDecodeStrict' v of
      Left err  -> Left . ColumnErrorInternal $ "JSON parsing failed: " <> err
      Right val -> pure val
    MySQLNull    -> Left ColumnUnexpectedNull
    v            -> throwTypeMismatch "Aeson.Value" t v

-- Helpers

tryPackDecimal :: (Integral a, Bounded a) =>
  String -> Scientific -> Either ColumnParseError a
tryPackDecimal typeName = maybe throwDecimalWon'tFit pure . toBoundedInteger
  where
    throwDecimalWon'tFit =
      Left . ColumnErrorInternal $ typeName <> " cannot store this DECIMAL"

throwTypeMismatch ::
  String -> FieldType -> MySQLValue -> Either ColumnParseError a
throwTypeMismatch typeName ft v =
  Left . ColumnTypeMismatch typeName (fieldTypeToString ft) $ value
  where
    value :: String
    value = "Value found: " +| show v |+ ""

-- Stringification of type names
fieldTypeToString :: FieldType -> String
fieldTypeToString ft = "MySQL " <>
  if | ft == mySQLTypeDecimal -> "Decimal"
     | ft == mySQLTypeTiny -> "Tiny"
     | ft == mySQLTypeShort -> "Short"
     | ft == mySQLTypeLong -> "Long"
     | ft == mySQLTypeFloat -> "Float"
     | ft == mySQLTypeDouble -> "Double"
     | ft == mySQLTypeNull -> "Null"
     | ft == mySQLTypeTimestamp -> "Timestamp"
     | ft == mySQLTypeLongLong -> "LongLong"
     | ft == mySQLTypeInt24 -> "Int24"
     | ft == mySQLTypeDate -> "Date"
     | ft == mySQLTypeTime -> "Time"
     | ft == mySQLTypeDateTime -> "DateTime"
     | ft == mySQLTypeYear -> "Year"
     | ft == mySQLTypeNewDate -> "NewDate"
     | ft == mySQLTypeVarChar -> "VarChar"
     | ft == mySQLTypeBit -> "Bit"
     | ft == mySQLTypeTimestamp2 -> "Timestamp2"
     | ft == mySQLTypeDateTime2 -> "DateTime2"
     | ft == mySQLTypeTime2 -> "Time2"
     | ft == mySQLTypeNewDecimal -> "NewDecimal"
     | ft == mySQLTypeEnum -> "Enum"
     | ft == mySQLTypeSet -> "Set"
     | ft == mySQLTypeTinyBlob -> "TinyBlob"
     | ft == mySQLTypeMediumBlob -> "MediumBlob"
     | ft == mySQLTypeLongBlob -> "LongBlob"
     | ft == mySQLTypeBlob -> "Blob"
     | ft == mySQLTypeVarString -> "VarString"
     | ft == mySQLTypeString -> "String"
     | otherwise -> "Geometry" -- brittle, to fix
      -}
