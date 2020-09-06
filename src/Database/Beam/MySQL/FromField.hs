{-# LANGUAGE AllowAmbiguousTypes    #-}
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
import           Data.Text (Text, pack, unpack)
import           Data.Text.Encoding (encodeUtf8)
import           Data.Time (Day, LocalTime (LocalTime), NominalDiffTime,
                            TimeOfDay, daysAndTimeOfDayToTime, midnight)
import           Data.Word (Word16, Word32, Word64, Word8)
import           Database.Beam.Backend.SQL (SqlNull (SqlNull))
import           Database.MySQL.Base (MySQLValue (..))
import           Text.Read (readMaybe)
import           Type.Reflection (Typeable, tyConName, typeRep, typeRepTyCon)

data Leniency = Lenient | Strict
  deriving stock (Eq, Show)

data FromFieldResult (l :: Leniency) (a :: Type) where
  UnexpectedNull :: {-# UNPACK #-} !Text -> FromFieldResult l a
  TypeMismatch :: {-# UNPACK #-} !Text -> FromFieldResult l a
  Won'tFit :: {-# UNPACK #-} !Text -> FromFieldResult l a
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
    TypeMismatch t -> case v of
      MySQLText v'   -> tryText v'
      MySQLDouble v' -> tryIEEE v'
      MySQLFloat v'  -> tryIEEE v'
      _              -> TypeMismatch t
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
    TypeMismatch t -> case v of
      MySQLText v'   -> tryText v'
      MySQLDouble v' -> tryIEEE v'
      MySQLFloat v'  -> tryIEEE v'
      _              -> TypeMismatch t
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
    TypeMismatch t -> case v of
      MySQLText v'   -> tryText v'
      MySQLDouble v' -> tryIEEE v'
      MySQLFloat v'  -> tryIEEE v'
      _              -> TypeMismatch t
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
    TypeMismatch t -> case v of
      MySQLText v'   -> tryText v'
      MySQLDouble v' -> tryIEEE v'
      MySQLFloat v'  -> tryIEEE v'
      _              -> TypeMismatch t
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
    TypeMismatch t -> case v of
      MySQLText v'   -> tryText v'
      MySQLDouble v' -> tryIEEE v'
      MySQLFloat v'  -> tryIEEE v'
      _              -> TypeMismatch t
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
    TypeMismatch t -> case v of
      MySQLText v' -> tryIEEEFromText v'
      _            -> TypeMismatch t
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
    TypeMismatch t -> case v of
      MySQLText v' -> tryIEEEFromText v'
      _            -> TypeMismatch t
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
    {-
     - This is fairly uninformative right now.
     - However, due to how NULLable fields are parsed, that would be tricky.
     - Thanks Beam! - Koz
     -}
    _ -> TypeMismatch "A non-null"

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
    TypeMismatch t -> case v of
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
      _               -> TypeMismatch t
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
      else TypeMismatch "TimeOfDay" -- TODO: More informative error. - Koz
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

-- Helpers

typeName :: forall (a :: Type) .
  (Typeable a) => Text
typeName = pack . tyConName . typeRepTyCon $ (typeRep @a)

handleNullOrMismatch :: forall (a :: Type) (l :: Leniency) .
  (Typeable a) =>
  MySQLValue -> FromFieldResult l a
handleNullOrMismatch = \case
  MySQLNull -> UnexpectedNull (typeName @a)
  _ -> TypeMismatch (typeName @a)

relax :: FromFieldResult 'Strict a -> FromFieldResult 'Lenient a
relax = \case
  UnexpectedNull t -> UnexpectedNull t
  TypeMismatch t -> TypeMismatch t
  Won'tFit t -> Won'tFit t
  StrictParse x -> StrictParse x

tryScientific :: forall (a :: Type) .
  (Typeable a, Integral a, Bounded a) =>
  Scientific -> FromFieldResult 'Strict a
tryScientific = maybe (Won'tFit (typeName @a)) StrictParse . toBoundedInteger

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

tryFixed :: forall (b :: Type) (a :: Type) .
  (Typeable b, Integral a, Integral b, Bits a, Bits b) =>
  a -> FromFieldResult 'Strict b
tryFixed = maybe (Won'tFit (typeName @b)) StrictParse . toIntegralSized

tryIEEEFromText :: (Read a, RealFloat a) =>
  Text -> FromFieldResult 'Lenient a
tryIEEEFromText = \case
  "Infinity" -> LenientParse . read $ "Infinity"
  "-Infinity" -> LenientParse . read $ "-Infinity"
  t -> case readMaybe . unpack $ t of
    Nothing -> TextCouldNotParse
    Just v -> if isInfinite v
              then case signum v of
                -1 -> IEEETooSmall
                _  -> IEEETooBig
              else LenientParse v
