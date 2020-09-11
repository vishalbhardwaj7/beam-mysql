{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Beam.MySQL.FromField where

import           Data.Bits (Bits (zeroBits), toIntegralSized)
import           Data.ByteString (ByteString)
import           Data.Int (Int16, Int32, Int64, Int8)
import           Data.Kind (Type)
import           Data.Scientific (Scientific, toBoundedInteger)
import           Data.Text (Text, pack, unpack)
import           Data.Text.Encoding (encodeUtf8)
import           Data.Time (Day, LocalTime (LocalTime), TimeOfDay, midnight)
import           Data.Word (Word16, Word32, Word64, Word8)
import           Database.Beam.Backend.SQL (SqlNull (SqlNull))
import           Database.MySQL.Base (MySQLValue (..))
import           Text.Read (readMaybe)
import           Type.Reflection (TyCon, Typeable, typeRep, typeRepTyCon)

data Strict =
  UnexpectedNull |
  TypeMismatch |
  Won'tFit
  deriving stock (Eq, Show)

data Lenient =
  SomeStrict !Strict |
  IEEENaN |
  IEEEInfinity |
  IEEETooSmall |
  IEEETooBig |
  TextCouldNotParse
  deriving stock (Eq, Show)

data DecodeError (e :: Type) =
  DecodeError !e {-# UNPACK #-} !TyCon
  deriving stock (Eq, Show)

class FromFieldStrict (a :: Type) where
  fromFieldStrict :: MySQLValue -> Either (DecodeError Strict) a

instance FromFieldStrict Bool where
  {-# INLINABLE fromFieldStrict #-}
  fromFieldStrict = \case
    MySQLInt8 v -> Right (zeroBits /= v)
    MySQLInt8U v -> Right (zeroBits /= v)
    MySQLBit v -> Right (zeroBits /= v)
    v -> handleNullOrMismatch v

instance FromFieldStrict Int8 where
  {-# INLINABLE fromFieldStrict #-}
  fromFieldStrict = \case
    MySQLInt8 v -> Right v
    MySQLDecimal v -> tryScientific v
    v -> handleNullOrMismatch v

instance FromFieldStrict Int16 where
  {-# INLINABLE fromFieldStrict #-}
  fromFieldStrict = \case
    MySQLInt8 v -> Right . fromIntegral $ v
    MySQLInt16 v -> Right v
    MySQLDecimal v -> tryScientific v
    v -> handleNullOrMismatch v

instance FromFieldStrict Int32 where
  {-# INLINABLE fromFieldStrict #-}
  fromFieldStrict = \case
    MySQLInt8 v -> Right . fromIntegral $ v
    MySQLInt16 v -> Right . fromIntegral $ v
    MySQLInt32 v -> Right v
    MySQLDecimal v -> tryScientific v
    v -> handleNullOrMismatch v

instance FromFieldStrict Int64 where
  {-# INLINABLE fromFieldStrict #-}
  fromFieldStrict = \case
    MySQLInt8 v -> Right . fromIntegral $ v
    MySQLInt16 v -> Right . fromIntegral $ v
    MySQLInt32 v -> Right . fromIntegral $ v
    MySQLInt64 v -> Right v
    MySQLDecimal v -> tryScientific v
    v -> handleNullOrMismatch v

instance FromFieldStrict Int where
  {-# INLINABLE fromFieldStrict #-}
  fromFieldStrict = \case
    MySQLInt8 v -> Right . fromIntegral $ v
    MySQLInt16 v -> Right . fromIntegral $ v
    MySQLInt32 v -> tryFixed v
    MySQLInt64 v -> tryFixed v
    MySQLDecimal v -> tryScientific v
    v -> handleNullOrMismatch v

instance FromFieldStrict Word8 where
  {-# INLINABLE fromFieldStrict #-}
  fromFieldStrict = \case
    MySQLInt8U v -> Right v
    MySQLDecimal v -> tryScientific v
    v -> handleNullOrMismatch v

instance FromFieldStrict Word16 where
  {-# INLINABLE fromFieldStrict #-}
  fromFieldStrict = \case
    MySQLInt8U v -> Right . fromIntegral $ v
    MySQLInt16U v -> Right v
    MySQLDecimal v -> tryScientific v
    v -> handleNullOrMismatch v

instance FromFieldStrict Word32 where
  {-# INLINABLE fromFieldStrict #-}
  fromFieldStrict = \case
    MySQLInt8U v -> Right . fromIntegral $ v
    MySQLInt16U v -> Right . fromIntegral $ v
    MySQLInt32U v -> Right v
    MySQLDecimal v -> tryScientific v
    v -> handleNullOrMismatch v

instance FromFieldStrict Word64 where
  {-# INLINABLE fromFieldStrict #-}
  fromFieldStrict = \case
    MySQLInt8U v -> Right . fromIntegral $ v
    MySQLInt16U v -> Right . fromIntegral $ v
    MySQLInt32U v -> Right . fromIntegral $ v
    MySQLInt64U v -> Right v
    MySQLDecimal v -> tryScientific v
    v -> handleNullOrMismatch v

instance FromFieldStrict Word where
  {-# INLINABLE fromFieldStrict #-}
  fromFieldStrict = \case
    MySQLInt8U v -> Right . fromIntegral $ v
    MySQLInt16U v -> Right . fromIntegral $ v
    MySQLInt32U v -> tryFixed v
    MySQLInt64U v -> tryFixed v
    MySQLDecimal v -> tryScientific v
    v -> handleNullOrMismatch v

instance FromFieldStrict Float where
  {-# INLINABLE fromFieldStrict #-}
  fromFieldStrict = \case
    MySQLFloat v -> Right v
    v -> handleNullOrMismatch v

instance FromFieldStrict Double where
  {-# INLINABLE fromFieldStrict #-}
  fromFieldStrict = \case
    MySQLFloat v -> Right . realToFrac $ v
    MySQLDouble v -> Right v
    v -> handleNullOrMismatch v

instance FromFieldStrict SqlNull where
  {-# INLINABLE fromFieldStrict #-}
  fromFieldStrict = \case
    MySQLNull -> Right SqlNull
    _ -> Left . DecodeError TypeMismatch . typeRepTyCon $ (typeRep @SqlNull)

instance FromFieldStrict ByteString where
  {-# INLINABLE fromFieldStrict #-}
  fromFieldStrict = \case
    MySQLText v -> Right . encodeUtf8 $ v
    MySQLBytes v -> Right v
    v -> handleNullOrMismatch v

instance FromFieldStrict Text where
  {-# INLINABLE fromFieldStrict #-}
  fromFieldStrict = \case
    MySQLText v -> Right v
    v -> handleNullOrMismatch v

instance FromFieldStrict LocalTime where
  {-# INLINABLE fromFieldStrict #-}
  fromFieldStrict = \case
    MySQLDateTime v -> Right v
    MySQLTimeStamp v -> Right v
    MySQLDate v -> Right . LocalTime v $ midnight
    v -> handleNullOrMismatch v

instance FromFieldStrict Day where
  {-# INLINABLE fromFieldStrict #-}
  fromFieldStrict = \case
    MySQLDate v -> Right v
    v -> handleNullOrMismatch v

instance FromFieldStrict TimeOfDay where
  {-# INLINABLE fromFieldStrict #-}
  fromFieldStrict = \case
    MySQLTime s v ->
      if s == zeroBits
      then Right v
      else
        Left . DecodeError TypeMismatch . typeRepTyCon $ (typeRep @TimeOfDay)
    v -> handleNullOrMismatch v

class (FromFieldStrict a) => FromFieldLenient (a :: Type) where
  {-# INLINABLE fromFieldLenient #-}
  fromFieldLenient :: MySQLValue -> Either (DecodeError Lenient) a
  fromFieldLenient v = case fromFieldStrict v of
    Left (DecodeError err t) -> Left . DecodeError (SomeStrict err) $ t
    Right res                -> Right res

instance (FromFieldStrict a,
          Read a,
          Typeable a,
          Integral a,
          Bounded a) =>
  FromFieldLenient a where
  {-# INLINABLE fromFieldLenient #-}
  fromFieldLenient v = case fromFieldStrict v of
    Left (DecodeError err t)    -> case err of
      TypeMismatch -> case v of
        MySQLText v'   -> tryText v'
        MySQLFloat v'  -> tryIEEE v'
        MySQLDouble v' -> tryIEEE v'
        _              -> Left . DecodeError (SomeStrict err) $ t
      _            -> Left . DecodeError (SomeStrict err) $ t
    Right res -> Right res

instance FromFieldLenient Float where
  {-# INLINABLE fromFieldLenient #-}
  fromFieldLenient v = case fromFieldStrict v of
    Left (DecodeError err t) -> case err of
      TypeMismatch -> case v of
        MySQLText v' -> tryIEEEFromText v'
        _            -> Left . DecodeError (SomeStrict err) $ t
      _ -> Left . DecodeError (SomeStrict err) $ t
    Right res -> Right res

instance FromFieldLenient Double where
  {-# INLINABLE fromFieldLenient #-}
  fromFieldLenient v = case fromFieldStrict v of
    Left (DecodeError err t) -> case err of
      TypeMismatch -> case v of
        MySQLText v' -> tryIEEEFromText v'
        _            -> Left . DecodeError (SomeStrict err) $ t
      _ -> Left . DecodeError (SomeStrict err) $ t
    Right res -> Right res

instance FromFieldLenient Text where
  {-# INLINABLE fromFieldLenient #-}
  fromFieldLenient v = case fromFieldStrict v of
    Left (DecodeError err t) -> case err of
      TypeMismatch -> case v of
        MySQLInt8 v'    -> Right . pack . show $ v'
        MySQLInt8U v'   -> Right . pack . show $ v'
        MySQLInt16 v'   -> Right . pack . show $ v'
        MySQLInt16U v'  -> Right . pack . show $ v'
        MySQLInt32 v'   -> Right . pack . show $ v'
        MySQLInt32U v'  -> Right . pack . show $ v'
        MySQLInt64 v'   -> Right . pack . show $ v'
        MySQLInt64U v'  -> Right . pack . show $ v'
        MySQLDecimal v' -> Right . pack . show $ v'
        MySQLFloat v'   -> Right . pack . show $ v'
        MySQLDouble v'  -> Right . pack . show $ v'
        _               -> Left . DecodeError (SomeStrict err) $ t
      _ -> Left . DecodeError (SomeStrict err) $ t
    Right res -> Right res

-- Helpers

handleNullOrMismatch :: forall (a :: Type) .
  (Typeable a) =>
  MySQLValue -> Either (DecodeError Strict) a
handleNullOrMismatch = Left . \case
  MySQLNull -> DecodeError UnexpectedNull (tyCon @a)
  _ -> DecodeError TypeMismatch (tyCon @a)

tryScientific :: forall (a :: Type) .
  (Typeable a, Integral a, Bounded a) =>
  Scientific -> Either (DecodeError Strict) a
tryScientific s = case toBoundedInteger s of
  Nothing -> Left . DecodeError Won'tFit $ (tyCon @a)
  Just v  -> Right v

tryText :: forall (a :: Type) .
  (Read a, Typeable a) =>
  Text -> Either (DecodeError Lenient) a
tryText t = case readMaybe . unpack $ t of
  Nothing -> Left . DecodeError TextCouldNotParse $ (tyCon @a)
  Just x  -> Right x

tryIEEE :: forall (b :: Type) (a :: Type) .
  (Integral b, RealFloat a, Bounded b, Typeable b) =>
  a -> Either (DecodeError Lenient) b
tryIEEE v
  | isNaN v = Left . DecodeError IEEENaN $ (tyCon @b)
  | isInfinite v = Left . DecodeError IEEEInfinity $ (tyCon @b)
  | v < fromIntegral (minBound @b) =
      Left. DecodeError IEEETooSmall $ (tyCon @b)
  | v > fromIntegral (maxBound @b) =
      Left . DecodeError IEEETooBig $ (tyCon @b)
  | otherwise = Right . truncate $ v

tryIEEEFromText :: forall (a :: Type) .
  (Read a, RealFloat a, Typeable a) =>
  Text -> Either (DecodeError Lenient) a
tryIEEEFromText = \case
  "Infinity" -> Right . read $ "Infinity"
  "-Infinity" -> Right . read $ "-Infinity"
  t -> case readMaybe . unpack $ t of
    Nothing -> Left . DecodeError TextCouldNotParse $ (tyCon @a)
    Just v -> if isInfinite v
                then case signum v of
                  -1 -> Left . DecodeError IEEETooSmall $ (tyCon @a)
                  _  -> Left . DecodeError IEEETooBig $ (tyCon @a)
                else Right v

tryFixed :: forall (b :: Type) (a :: Type) .
  (Integral a, Integral b, Bits a, Bits b, Typeable b) =>
  a -> Either (DecodeError Strict) b
tryFixed v = case toIntegralSized v of
  Nothing -> Left . DecodeError Won'tFit $ (tyCon @b)
  Just v' -> Right v'

tyCon :: forall (a :: Type) .
  (Typeable a) =>
  TyCon
tyCon = typeRepTyCon (typeRep @a)
