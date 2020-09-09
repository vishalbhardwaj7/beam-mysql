module Database.Beam.MySQL.Syntax.Value where

import           Data.Bool (bool)
import           Data.ByteString (ByteString)
import           Data.Int (Int16, Int32, Int64, Int8)
import           Data.Scientific (Scientific)
import           Data.Text (Text)
import           Data.Time (Day, LocalTime, TimeOfDay)
import           Data.Word (Word16, Word32, Word64, Word8)
import           Database.Beam.Backend.SQL (HasSqlValueSyntax (sqlValueSyntax),
                                            SqlNull)
import           Database.MySQL.Base (MySQLValue (..), Param (One),
                                      QueryParam (render))
import           Generic.Data (gcompare)

-- A user-provided parameter
newtype P = P MySQLValue
  deriving newtype (Eq)
  deriving stock (Show)

instance Ord P where
  {-# INLINE compare #-}
  compare (P val) (P val2) = gcompare val val2

instance QueryParam P where
  {-# INLINABLE render #-}
  render (P val) = render . One $ val

newtype MySQLValueSyntax = MySQLValueSyntax P
  deriving stock (Show)
  deriving newtype (Eq)

intoValueSyntax :: (a -> MySQLValue) -> a -> MySQLValueSyntax
intoValueSyntax f = MySQLValueSyntax . P . f

instance HasSqlValueSyntax MySQLValueSyntax Bool where
  {-# INLINABLE sqlValueSyntax #-}
  sqlValueSyntax = intoValueSyntax (MySQLInt8 . bool 0 1)

instance HasSqlValueSyntax MySQLValueSyntax Int8 where
  {-# INLINABLE sqlValueSyntax #-}
  sqlValueSyntax = intoValueSyntax MySQLInt8

instance HasSqlValueSyntax MySQLValueSyntax Int16 where
  {-# INLINABLE sqlValueSyntax #-}
  sqlValueSyntax = intoValueSyntax MySQLInt16

instance HasSqlValueSyntax MySQLValueSyntax Int32 where
  {-# INLINABLE sqlValueSyntax #-}
  sqlValueSyntax = intoValueSyntax MySQLInt32

instance HasSqlValueSyntax MySQLValueSyntax Int64 where
  {-# INLINABLE sqlValueSyntax #-}
  sqlValueSyntax = intoValueSyntax MySQLInt64

instance HasSqlValueSyntax MySQLValueSyntax Int where
  {-# INLINABLE sqlValueSyntax #-}
  sqlValueSyntax = intoValueSyntax (MySQLInt64 . fromIntegral)

instance HasSqlValueSyntax MySQLValueSyntax Word8 where
  {-# INLINABLE sqlValueSyntax #-}
  sqlValueSyntax = intoValueSyntax MySQLInt8U

instance HasSqlValueSyntax MySQLValueSyntax Word16 where
  {-# INLINABLE sqlValueSyntax #-}
  sqlValueSyntax = intoValueSyntax MySQLInt16U

instance HasSqlValueSyntax MySQLValueSyntax Word32 where
  {-# INLINABLE sqlValueSyntax #-}
  sqlValueSyntax = intoValueSyntax MySQLInt32U

instance HasSqlValueSyntax MySQLValueSyntax Word64 where
  {-# INLINABLE sqlValueSyntax #-}
  sqlValueSyntax = intoValueSyntax MySQLInt64U

instance HasSqlValueSyntax MySQLValueSyntax Word where
  {-# INLINABLE sqlValueSyntax #-}
  sqlValueSyntax = intoValueSyntax (MySQLInt64U . fromIntegral)

instance HasSqlValueSyntax MySQLValueSyntax Scientific where
  {-# INLINABLE sqlValueSyntax #-}
  sqlValueSyntax = intoValueSyntax MySQLDecimal

instance (HasSqlValueSyntax MySQLValueSyntax a) =>
  HasSqlValueSyntax MySQLValueSyntax (Maybe a) where
  {-# INLINABLE sqlValueSyntax #-}
  sqlValueSyntax = \case
    Nothing -> MySQLValueSyntax . P $ MySQLNull
    Just x -> sqlValueSyntax x

instance HasSqlValueSyntax MySQLValueSyntax SqlNull where
  {-# INLINABLE sqlValueSyntax #-}
  sqlValueSyntax _ = MySQLValueSyntax . P $ MySQLNull

instance HasSqlValueSyntax MySQLValueSyntax ByteString where
  {-# INLINABLE sqlValueSyntax #-}
  sqlValueSyntax = intoValueSyntax MySQLBytes

instance HasSqlValueSyntax MySQLValueSyntax Text where
  {-# INLINABLE sqlValueSyntax #-}
  sqlValueSyntax = intoValueSyntax MySQLText

instance HasSqlValueSyntax MySQLValueSyntax Day where
  {-# INLINABLE sqlValueSyntax #-}
  sqlValueSyntax = intoValueSyntax MySQLDate

instance HasSqlValueSyntax MySQLValueSyntax TimeOfDay where
  {-# INLINABLE sqlValueSyntax #-}
  sqlValueSyntax = intoValueSyntax (MySQLTime 0)

instance HasSqlValueSyntax MySQLValueSyntax LocalTime where
  {-# INLINABLE sqlValueSyntax #-}
  sqlValueSyntax = intoValueSyntax MySQLDateTime

-- TODO: NominalDiffTime - the target is MySQLDateTime, with appropriate sign. -
-- Koz

