module Database.Beam.MySQL.Syntax.Value where

import           Data.ByteString (ByteString)
import           Data.Int (Int16, Int32, Int64, Int8)
import           Data.Scientific (Scientific)
import           Data.Text (Text)
import           Data.Time (Day, LocalTime, NominalDiffTime, TimeOfDay)
import           Data.Word (Word16, Word32, Word64, Word8)
import           Database.Beam.Backend.SQL (HasSqlValueSyntax (sqlValueSyntax),
                                            SqlNull)

data MySQLValueSyntax =
  VBool !Bool |
  VInt8 {-# UNPACK #-} !Int8 |
  VInt16 {-# UNPACK #-} !Int16 |
  VInt32 {-# UNPACK #-} !Int32 |
  VInt64 {-# UNPACK #-} !Int64 |
  VWord8 {-# UNPACK #-} !Word8 |
  VWord16 {-# UNPACK #-} !Word16 |
  VWord32 {-# UNPACK #-} !Word32 |
  VWord64 {-# UNPACK #-} !Word64 |
  VScientific {-# UNPACK #-} !Scientific |
  VNothing | -- Missing value
  VNull | -- SQL NULL
  VByteString {-# UNPACK #-} !ByteString |
  VText {-# UNPACK #-} !Text |
  VDay !Day |
  VLocalTime {-# UNPACK #-} !LocalTime |
  VTimeOfDay {-# UNPACK #-} !TimeOfDay |
  VNominalDiffTime !NominalDiffTime
  deriving stock (Eq, Show)

instance HasSqlValueSyntax MySQLValueSyntax Bool where
  {-# INLINABLE sqlValueSyntax #-}
  sqlValueSyntax = VBool

instance HasSqlValueSyntax MySQLValueSyntax Int8 where
  {-# INLINABLE sqlValueSyntax #-}
  sqlValueSyntax = VInt8

instance HasSqlValueSyntax MySQLValueSyntax Int16 where
  {-# INLINABLE sqlValueSyntax #-}
  sqlValueSyntax = VInt16

instance HasSqlValueSyntax MySQLValueSyntax Int32 where
  {-# INLINABLE sqlValueSyntax #-}
  sqlValueSyntax = VInt32

instance HasSqlValueSyntax MySQLValueSyntax Int64 where
  {-# INLINABLE sqlValueSyntax #-}
  sqlValueSyntax = VInt64

instance HasSqlValueSyntax MySQLValueSyntax Int where
  {-# INLINABLE sqlValueSyntax #-}
  sqlValueSyntax = VInt64 . fromIntegral

instance HasSqlValueSyntax MySQLValueSyntax Word8 where
  {-# INLINABLE sqlValueSyntax #-}
  sqlValueSyntax = VWord8

instance HasSqlValueSyntax MySQLValueSyntax Word16 where
  {-# INLINABLE sqlValueSyntax #-}
  sqlValueSyntax = VWord16

instance HasSqlValueSyntax MySQLValueSyntax Word32 where
  {-# INLINABLE sqlValueSyntax #-}
  sqlValueSyntax = VWord32

instance HasSqlValueSyntax MySQLValueSyntax Word64 where
  {-# INLINABLE sqlValueSyntax #-}
  sqlValueSyntax = VWord64

instance HasSqlValueSyntax MySQLValueSyntax Word where
  {-# INLINABLE sqlValueSyntax #-}
  sqlValueSyntax = VWord64 . fromIntegral

instance HasSqlValueSyntax MySQLValueSyntax Scientific where
  {-# INLINABLE sqlValueSyntax #-}
  sqlValueSyntax = VScientific

instance (HasSqlValueSyntax MySQLValueSyntax a) =>
  HasSqlValueSyntax MySQLValueSyntax (Maybe a) where
  {-# INLINABLE sqlValueSyntax #-}
  sqlValueSyntax = \case
    Nothing -> VNothing
    Just x -> sqlValueSyntax x

instance HasSqlValueSyntax MySQLValueSyntax SqlNull where
  {-# INLINABLE sqlValueSyntax #-}
  sqlValueSyntax = const VNull

instance HasSqlValueSyntax MySQLValueSyntax ByteString where
  {-# INLINABLE sqlValueSyntax #-}
  sqlValueSyntax = VByteString

instance HasSqlValueSyntax MySQLValueSyntax Text where
  {-# INLINABLE sqlValueSyntax #-}
  sqlValueSyntax = VText

instance HasSqlValueSyntax MySQLValueSyntax Day where
  {-# INLINABLE sqlValueSyntax #-}
  sqlValueSyntax = VDay

instance HasSqlValueSyntax MySQLValueSyntax TimeOfDay where
  {-# INLINABLE sqlValueSyntax #-}
  sqlValueSyntax = VTimeOfDay

instance HasSqlValueSyntax MySQLValueSyntax LocalTime where
  {-# INLINABLE sqlValueSyntax #-}
  sqlValueSyntax = VLocalTime

instance HasSqlValueSyntax MySQLValueSyntax NominalDiffTime where
  {-# INLINABLE sqlValueSyntax #-}
  sqlValueSyntax = VNominalDiffTime
