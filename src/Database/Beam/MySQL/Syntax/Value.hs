module Database.Beam.MySQL.Syntax.Value where

import           Data.Bool (bool)
import           Data.ByteString (ByteString)
import           Data.Char (isLatin1)
import           Data.Int (Int16, Int32, Int64, Int8)
import           Data.Scientific (Scientific)
import           Data.Text (Text, foldl')
import           Data.Time (Day, LocalTime, TimeOfDay)
import qualified Data.Vector as V
import           Data.Word (Word16, Word32, Word64, Word8)
import           Database.Beam.Backend.SQL (HasSqlValueSyntax (sqlValueSyntax),
                                            SqlNull)
import           Database.Beam.MySQL.Syntax.Render (RenderError (NotLatin1),
                                                    RenderInto (renderPass),
                                                    RenderResult (RenderResult))
import           Database.MySQL.Base (MySQLValue (..), Param (One))

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
  VTimeOfDay {-# UNPACK #-} !TimeOfDay
  deriving stock (Eq, Show)

instance RenderInto RenderError RenderResult MySQLValueSyntax where
  {-# INLINABLE renderPass #-}
  renderPass = \case
    VBool b -> intoRenderResult (MySQLInt8 . bool 0 1) b
    VInt8 i -> intoRenderResult MySQLInt8 i
    VInt16 i -> intoRenderResult MySQLInt16 i
    VInt32 i -> intoRenderResult MySQLInt32 i
    VInt64 i -> intoRenderResult MySQLInt64 i
    VWord8 w -> intoRenderResult MySQLInt8U w
    VWord16 w -> intoRenderResult MySQLInt16U w
    VWord32 w -> intoRenderResult MySQLInt32U w
    VWord64 w -> intoRenderResult MySQLInt64U w
    VScientific s -> intoRenderResult MySQLDecimal s
    -- We can put NULLs straight in.
    VNothing -> Right . RenderResult "NULL" mempty $ mempty
    VNull -> Right . RenderResult "NULL" mempty $ mempty
    VByteString b -> intoRenderResult MySQLBytes b
    -- We have to do a Latin-1 encoding check here, due to current limitations
    -- in our database. - Koz
    VText t -> case foldl' go Nothing t of
      -- Our text is Latin-1, so we're good.
      Nothing -> intoRenderResult MySQLText t
      -- Our text is _not_ Latin-1, so throw.
      Just () -> Left (NotLatin1 t)
      where
        go :: Maybe () -> Char -> Maybe ()
        go acc c = case acc of
          Nothing -> if isLatin1 c
                      then Nothing
                      else Just ()
          Just () -> Just ()
    VDay d -> intoRenderResult MySQLDate d
    VLocalTime lt -> intoRenderResult MySQLTimeStamp lt
    VTimeOfDay tod -> intoRenderResult (MySQLTime 0) tod

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

-- Helpers

intoRenderResult :: (a -> MySQLValue) -> a -> Either e RenderResult
intoRenderResult f x =
  Right . RenderResult "?" (V.singleton . One . f $ x) $ mempty
