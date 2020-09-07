{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE UndecidableInstances   #-}

module Database.Beam.MySQL.FromField where

import           Data.Bits (Bits (zeroBits), toIntegralSized)
import           Data.ByteString (ByteString)
import           Data.Int (Int16, Int32, Int64, Int8)
import           Data.Kind (Type)
import           Data.Scientific (Scientific, toBoundedInteger)
import           Data.Text (Text, pack, unpack)
import           Data.Text.Encoding (encodeUtf8)
import           Data.Time (Day, LocalTime (LocalTime), NominalDiffTime,
                            TimeOfDay, daysAndTimeOfDayToTime, midnight)
import           Data.Word (Word16, Word32, Word64, Word8)
import           Database.Beam.Backend.SQL (SqlNull (SqlNull))
import           Database.MySQL.Base (MySQLValue (..))
import           Text.Read (readMaybe)
import           Type.Reflection (TyCon, Typeable, tyConName, typeRep,
                                  typeRepTyCon)

data Leniency = Lenient | Strict
  deriving stock (Eq, Show)

data FromFieldResult (l :: Leniency) (a :: Type) where
  UnexpectedNull' :: FromFieldResult l a
  TypeMismatch' :: FromFieldResult l a
  Won'tFit' :: FromFieldResult l a
  IEEENaN' :: FromFieldResult 'Lenient a
  IEEEInfinity' :: FromFieldResult 'Lenient a
  IEEETooSmall' :: FromFieldResult 'Lenient a
  IEEETooBig' :: FromFieldResult 'Lenient a
  TextCouldNotParse' :: FromFieldResult 'Lenient a
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
    v -> handleNullOrMismatch v

instance FromField 'Lenient (L Bool) where
  {-# INLINABLE fromField #-}
  fromField = relax . fmap L . fromField

instance FromField 'Strict Int8 where
  {-# INLINABLE fromField #-}
  fromField = \case
    MySQLInt8 v -> StrictParse v
    MySQLDecimal v -> tryScientific v
    v -> handleNullOrMismatch v

instance FromField 'Lenient (L Int8) where
  {-# INLINABLE fromField #-}
  fromField v = fmap L $ case fromField v of
    TypeMismatch' -> case v of
      MySQLText v'   -> tryText v'
      MySQLDouble v' -> tryIEEE v'
      MySQLFloat v'  -> tryIEEE v'
      _              -> TypeMismatch'
    res          -> relax res

instance FromField 'Strict Int16 where
  {-# INLINABLE fromField #-}
  fromField = \case
    MySQLInt8 v -> StrictParse . fromIntegral $ v
    MySQLInt16 v -> StrictParse v
    MySQLDecimal v -> tryScientific v
    v -> handleNullOrMismatch v

instance FromField 'Lenient (L Int16) where
  {-# INLINABLE fromField #-}
  fromField v = fmap L $ case fromField v of
    TypeMismatch' -> case v of
      MySQLText v'   -> tryText v'
      MySQLDouble v' -> tryIEEE v'
      MySQLFloat v'  -> tryIEEE v'
      _              -> TypeMismatch'
    res -> relax res

instance FromField 'Strict Int32 where
  {-# INLINABLE fromField #-}
  fromField = \case
    MySQLInt8 v -> StrictParse . fromIntegral $ v
    MySQLInt16 v -> StrictParse . fromIntegral $ v
    MySQLInt32 v -> StrictParse v
    MySQLDecimal v -> tryScientific v
    v -> handleNullOrMismatch v

instance FromField 'Lenient (L Int32) where
  {-# INLINABLE fromField #-}
  fromField v = fmap L $ case fromField v of
    TypeMismatch' -> case v of
      MySQLText v'   -> tryText v'
      MySQLDouble v' -> tryIEEE v'
      MySQLFloat v'  -> tryIEEE v'
      _              -> TypeMismatch'
    res -> relax res

instance FromField 'Strict Int64 where
  {-# INLINABLE fromField #-}
  fromField = \case
    MySQLInt8 v -> StrictParse . fromIntegral $ v
    MySQLInt16 v -> StrictParse . fromIntegral $ v
    MySQLInt32 v -> StrictParse . fromIntegral $ v
    MySQLInt64 v -> StrictParse v
    MySQLDecimal v -> tryScientific v
    v -> handleNullOrMismatch v

instance FromField 'Lenient (L Int64) where
  {-# INLINABLE fromField #-}
  fromField v = fmap L $ case fromField v of
    TypeMismatch' -> case v of
      MySQLText v'   -> tryText v'
      MySQLDouble v' -> tryIEEE v'
      MySQLFloat v'  -> tryIEEE v'
      _              -> TypeMismatch'
    res -> relax res

instance FromField 'Strict Int where
  {-# INLINABLE fromField #-}
  fromField = \case
    MySQLInt8 v -> StrictParse . fromIntegral $ v
    MySQLInt16 v -> StrictParse . fromIntegral $ v
    MySQLInt32 v -> tryFixed v
    MySQLInt64 v -> tryFixed v
    MySQLDecimal v -> tryScientific v
    v -> handleNullOrMismatch v

instance FromField 'Lenient (L Int) where
  {-# INLINABLE fromField #-}
  fromField v = fmap L $ case fromField v of
    TypeMismatch' -> case v of
      MySQLText v'   -> tryText v'
      MySQLDouble v' -> tryIEEE v'
      MySQLFloat v'  -> tryIEEE v'
      _              -> TypeMismatch'
    res -> relax res

instance FromField 'Strict Word8 where
  {-# INLINABLE fromField #-}
  fromField = \case
    MySQLInt8U v -> StrictParse v
    MySQLDecimal v -> tryScientific v
    v -> handleNullOrMismatch v

instance FromField 'Lenient (L Word8) where
  {-# INLINABLE fromField #-}
  fromField = fmap L . relax . fromField

instance FromField 'Strict Word16 where
  {-# INLINABLE fromField #-}
  fromField = \case
    MySQLInt8U v -> StrictParse . fromIntegral $ v
    MySQLInt16U v -> StrictParse v
    MySQLDecimal v -> tryScientific v
    v -> handleNullOrMismatch v

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
    v -> handleNullOrMismatch v

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
    v -> handleNullOrMismatch v

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
    v -> handleNullOrMismatch v

instance FromField 'Lenient (L Word) where
  {-# INLINABLE fromField #-}
  fromField = fmap L . relax . fromField

instance FromField 'Strict Float where
  {-# INLINABLE fromField #-}
  fromField = \case
    MySQLFloat v -> StrictParse v
    v -> handleNullOrMismatch v

instance FromField 'Lenient (L Float) where
  {-# INLINABLE fromField #-}
  fromField v = fmap L $ case fromField v of
    TypeMismatch' -> case v of
      MySQLText v' -> tryIEEEFromText v'
      _            -> TypeMismatch'
    res          -> relax res

instance FromField 'Strict Double where
  {-# INLINABLE fromField #-}
  fromField = \case
    MySQLFloat v -> StrictParse . realToFrac $ v
    MySQLDouble v -> StrictParse v
    v -> handleNullOrMismatch v

instance FromField 'Lenient (L Double) where
  {-# INLINABLE fromField #-}
  fromField v = fmap L $ case fromField v of
    TypeMismatch' -> case v of
      MySQLText v' -> tryIEEEFromText v'
      _            -> TypeMismatch'
    res -> relax res

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
    v -> handleNullOrMismatch v

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
    v -> handleNullOrMismatch v

instance FromField 'Lenient (L Rational) where
  {-# INLINABLE fromField #-}
  fromField = fmap L . relax . fromField

instance FromField 'Strict SqlNull where
  {-# INLINABLE fromField #-}
  fromField = \case
    MySQLNull -> StrictParse SqlNull
    _ -> TypeMismatch'

instance FromField 'Lenient (L SqlNull) where
  {-# INLINABLE fromField #-}
  fromField = fmap L . relax . fromField

instance FromField 'Strict ByteString where
  {-# INLINABLE fromField #-}
  fromField = \case
    MySQLText v -> StrictParse . encodeUtf8 $ v
    MySQLBytes v -> StrictParse v
    v -> handleNullOrMismatch v

instance FromField 'Lenient (L ByteString) where
  {-# INLINABLE fromField #-}
  fromField = fmap L . relax . fromField

instance FromField 'Strict Text where
  {-# INLINABLE fromField #-}
  fromField = \case
    MySQLText v -> StrictParse v
    v -> handleNullOrMismatch v

instance FromField 'Lenient (L Text) where
  {-# INLINABLE fromField #-}
  fromField v = fmap L $ case fromField v of
    TypeMismatch' -> case v of
      MySQLInt8 v'    -> LenientParse . pack . show $ v'
      MySQLInt8U v'   -> LenientParse . pack . show $ v'
      MySQLInt16 v'   -> LenientParse . pack . show $ v'
      MySQLInt16U v'  -> LenientParse . pack . show $ v'
      MySQLInt32 v'   -> LenientParse . pack . show $ v'
      MySQLInt32U v'  -> LenientParse . pack . show $ v'
      MySQLInt64 v'   -> LenientParse . pack . show $ v'
      MySQLInt64U v'  -> LenientParse . pack . show $ v'
      MySQLDecimal v' -> LenientParse . pack . show $ v'
      MySQLFloat v'   -> LenientParse . pack . show $ v'
      MySQLDouble v'  -> LenientParse . pack . show $ v'
      _               -> TypeMismatch'
    res          -> relax res

instance FromField 'Strict LocalTime where
  {-# INLINABLE fromField #-}
  fromField = \case
    MySQLDateTime v -> StrictParse v
    MySQLTimeStamp v -> StrictParse v
    MySQLDate v -> StrictParse . LocalTime v $ midnight
    v -> handleNullOrMismatch v

instance FromField 'Lenient (L LocalTime) where
  {-# INLINABLE fromField #-}
  fromField = fmap L . relax . fromField

instance FromField 'Strict Day where
  {-# INLINABLE fromField #-}
  fromField = \case
    MySQLDate v -> StrictParse v
    v -> handleNullOrMismatch v

instance FromField 'Lenient (L Day) where
  {-# INLINABLE fromField #-}
  fromField = fmap L . relax . fromField

instance FromField 'Strict TimeOfDay where
  {-# INLINABLE fromField #-}
  fromField = \case
    MySQLTime s v ->
      if s == zeroBits
      then StrictParse v
      else TypeMismatch' -- TODO: More informative error. - Koz
    v -> handleNullOrMismatch v

instance FromField 'Lenient (L TimeOfDay) where
  {-# INLINABLE fromField #-}
  fromField = fmap L . relax . fromField

instance FromField 'Strict NominalDiffTime where
  {-# INLINABLE fromField #-}
  fromField = \case
    MySQLTime s v ->
      let ndt = daysAndTimeOfDayToTime 0 v in
        if s == zeroBits
        then StrictParse ndt
        else StrictParse . negate $ ndt
    v -> handleNullOrMismatch v

instance FromField 'Lenient (L NominalDiffTime) where
  {-# INLINABLE fromField #-}
  fromField = fmap L . relax . fromField

data StrictDecodeError =
  UnexpectedNull {-# UNPACK #-} !TyCon |
  TypeMismatch {-# UNPACK #-} !TyCon |
  Won'tFit {-# UNPACK #-} !TyCon
  deriving stock (Eq, Show)

data LenientDecodeError =
  SomeStrictError !StrictDecodeError |
  IEEENaN {-# UNPACK #-} !TyCon |
  IEEEInfinity {-# UNPACK #-} !TyCon |
  IEEETooSmall {-# UNPACK #-} !TyCon |
  IEEETooBig {-# UNPACK #-} !TyCon |
  TextCouldNotParse {-# UNPACK #-} !TyCon
  deriving stock (Eq, Show)

-- WARNING: Do _not_ write any instances of these classes! They are a trick to
-- avoid having to define everything 'beam' demands of us twice.

class FromFieldS (a :: Type) where
  fromFieldS :: MySQLValue -> Either StrictDecodeError a

class FromFieldL (a :: Type) where
  fromFieldL :: MySQLValue -> Either LenientDecodeError a

instance (FromField 'Strict a, Typeable a) => FromFieldS a where
  {-# INLINABLE fromFieldS #-}
  fromFieldS v = case fromField v of
    UnexpectedNull' -> Left . UnexpectedNull $ tyConOf @a
    TypeMismatch'   -> Left . TypeMismatch $ tyConOf @a
    Won'tFit'       -> Left . Won'tFit $ tyConOf @a
    StrictParse v'  -> Right v'

instance (FromField 'Lenient (L a), Typeable a) => FromFieldL a where
  {-# INLINABLE fromFieldL #-}
  fromFieldL v = case fromField v of
    UnexpectedNull' -> Left . SomeStrictError . UnexpectedNull $ tyConOf @a
    TypeMismatch' -> Left . SomeStrictError . TypeMismatch $ tyConOf @a
    Won'tFit' -> Left . SomeStrictError . Won'tFit $ tyConOf @a
    IEEENaN' -> Left . IEEENaN $ tyConOf @a
    IEEEInfinity' -> Left . IEEEInfinity $ tyConOf @a
    IEEETooSmall' -> Left . IEEETooSmall $ tyConOf @a
    IEEETooBig' -> Left . IEEETooBig $ tyConOf @a
    TextCouldNotParse' -> Left . TextCouldNotParse $ tyConOf @a
    LenientParse (L v') -> Right v'
    StrictParse (L v') -> Right v'

-- Helpers

tyConOf :: forall (a :: Type) .
  (Typeable a) => TyCon
tyConOf = typeRepTyCon (typeRep @a)

typeName :: forall (a :: Type) .
  (Typeable a) => Text
typeName = pack . tyConName . typeRepTyCon $ (typeRep @a)

handleNullOrMismatch :: MySQLValue -> FromFieldResult l a
handleNullOrMismatch = \case
  MySQLNull -> UnexpectedNull'
  _ -> TypeMismatch'

relax :: FromFieldResult 'Strict a -> FromFieldResult 'Lenient a
relax = \case
  UnexpectedNull' -> UnexpectedNull'
  TypeMismatch' -> TypeMismatch'
  Won'tFit' -> Won'tFit'
  StrictParse x -> StrictParse x

tryScientific :: (Integral a, Bounded a) =>
  Scientific -> FromFieldResult 'Strict a
tryScientific = maybe Won'tFit' StrictParse . toBoundedInteger

tryText :: (Read a) => Text -> FromFieldResult 'Lenient a
tryText = maybe TextCouldNotParse' LenientParse . readMaybe . unpack

tryIEEE :: forall (b :: Type) (a :: Type) .
  (RealFloat a, Integral b, Bounded b) =>
  a -> FromFieldResult 'Lenient b
tryIEEE v
  | isNaN v = IEEENaN'
  | isInfinite v = IEEEInfinity'
  | v < fromIntegral (minBound @b) = IEEETooSmall'
  | v > fromIntegral (maxBound @b) = IEEETooBig'
  | otherwise = LenientParse . truncate $ v

tryFixed :: (Integral a, Integral b, Bits a, Bits b) =>
  a -> FromFieldResult 'Strict b
tryFixed = maybe Won'tFit' StrictParse . toIntegralSized

tryIEEEFromText :: (Read a, RealFloat a) =>
  Text -> FromFieldResult 'Lenient a
tryIEEEFromText = \case
  "Infinity" -> LenientParse . read $ "Infinity"
  "-Infinity" -> LenientParse . read $ "-Infinity"
  t -> case readMaybe . unpack $ t of
    Nothing -> TextCouldNotParse'
    Just v -> if isInfinite v
              then case signum v of
                -1 -> IEEETooSmall'
                _  -> IEEETooBig'
              else LenientParse v
