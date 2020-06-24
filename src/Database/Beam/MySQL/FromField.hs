{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Database.Beam.MySQL.FromField
    ( FieldParser
    , FromField(..)) where

import           Control.Applicative               ((<|>))
import           Control.Monad.Except              (ExceptT, throwError)
import           Data.Aeson                        (Value, eitherDecode')
import           Data.Attoparsec.ByteString.Char8  (Parser, char, decimal,
                                                    digit, double, parseOnly,
                                                    rational, signed,
                                                    takeByteString,
                                                    takeLazyByteString)
import           Data.Bits                         (finiteBitSize, zeroBits)
import qualified Data.ByteString                   as BS
import           Data.ByteString.Char8             (ByteString, uncons)
import qualified Data.ByteString.Lazy              as BSL
import           Data.Char                         (digitToInt)
import           Data.Int                          (Int16, Int32, Int64, Int8)
import           Data.Scientific                   (Scientific)
import           Data.Text                         (Text)
import           Data.Text.Encoding                (decodeUtf8')
import           Data.Time.Calendar                (Day, fromGregorianValid)
import           Data.Time.Clock                   (NominalDiffTime)
import           Data.Time.LocalTime               (LocalTime (..), TimeOfDay,
                                                    makeTimeOfDayValid)
import           Data.Word                         (Word16, Word32, Word64,
                                                    Word8)
import           Database.Beam.Backend.SQL         (SqlNull (..))
import           Database.Beam.Backend.SQL.Row     (ColumnParseError (..))
import           Database.MySQL.Protocol.ColumnDef (ColumnDef (columnType),
                                                    FieldType, mySQLTypeBit,
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

type FieldParser a = ExceptT ColumnParseError IO a

class FromField a where
    fromField :: ColumnDef -> Maybe ByteString -> FieldParser a

instance FromField Bool where
  fromField f = \case
    Nothing -> throwError ColumnUnexpectedNull
    md@(Just d) -> do
      let t = columnType f
      let st = fieldTypeToString t
      if | t == mySQLTypeTiny -> do
            n :: Int8 <- fromField f md
            if n == 0
            then pure False
            else pure True
         | t == mySQLTypeBit ->
            case uncons d of
              Nothing     -> throwError (ColumnTypeMismatch "Bool" st "empty bitstring")
              Just (h, _) -> if h == '\0'
                              then pure False
                              else pure True
         | otherwise -> throwError (ColumnTypeMismatch "Bool" st "")

instance FromField Int8 where
  fromField = parseChecking "Int8" fits8 (signed decimal)

instance FromField Int16 where
  fromField = parseChecking "Int16" fits16 (signed decimal)

instance FromField Int32 where
  fromField = parseChecking "Int32" fits32 (signed decimal)

instance FromField Int64 where
  fromField = parseChecking "Int64" fits64 (signed decimal)

instance FromField Int where
  fromField = parseChecking "Int" fitsWord (signed decimal)

instance FromField Word8 where
  fromField = parseChecking "Word8" fits8 decimal

instance FromField Word16 where
  fromField = parseChecking "Word16" fits16 decimal

instance FromField Word32 where
  fromField = parseChecking "Word32" fits32 decimal

instance FromField Word64 where
  fromField = parseChecking "Word64" fits64 decimal

instance FromField Word where
  fromField = parseChecking "Word" fitsWord decimal

instance FromField Float where
  fromField = parseChecking "Float" check (realToFrac <$> double)
    where
      check t =
        if | fits16 t -> True
           | t == mySQLTypeInt24 -> True
           | t == mySQLTypeFloat -> True
           | t == mySQLTypeDecimal -> True
           | t == mySQLTypeNewDecimal -> True
           | t == mySQLTypeDouble -> True
           | otherwise -> False

instance FromField Double where
  fromField = parseChecking "Double" check double
    where
      check t =
        if | fits32 t -> True
           | t == mySQLTypeFloat -> True
           | t == mySQLTypeDouble -> True
           | t == mySQLTypeDecimal -> True
           | t == mySQLTypeNewDecimal -> True
           | otherwise -> False

instance FromField Scientific where
  fromField = parseChecking "Scientific" representableScientific rational

instance FromField Rational where
  fromField = parseChecking "Rational" representableScientific rational

instance (FromField a) => FromField (Maybe a) where
  fromField f = \case
    Nothing -> pure Nothing
    d -> Just <$> fromField f d

instance FromField SqlNull where
  fromField f = \case
    Nothing -> pure SqlNull
    _ -> do
      let st = fieldTypeToString . columnType $ f
      throwError (ColumnTypeMismatch "SqlNull" st "Non-null value found")

instance FromField BS.ByteString where
  fromField = parseChecking "ByteString" representableBytes takeByteString

instance FromField BSL.ByteString where
  fromField = parseChecking "ByteString.Lazy" representableBytes takeLazyByteString

instance FromField Text where
  fromField = parseChecking "Text" representableText (go =<< takeByteString)
    where
      go bs = case decodeUtf8' bs of
        Left err -> fail . show $ err
        Right t  -> pure t

instance FromField LocalTime where
  fromField = parseChecking "LocalTime" check p
    where
      p = do
        (day, time) <- parseDayAndTime
        pure (LocalTime day time)
      check t =
        if | t == mySQLTypeDateTime -> True
           | t == mySQLTypeTimestamp -> True
           | t == mySQLTypeDate -> True
           | otherwise -> False

instance FromField Day where
  fromField = parseChecking "Day" (mySQLTypeDate ==) parseDay

instance FromField TimeOfDay where
  fromField = parseChecking "TimeOfDay" (mySQLTypeTime ==) parseTime

instance FromField NominalDiffTime where
  fromField = parseChecking "NominalDiffTime" (mySQLTypeTime ==) p
    where
      p = do
        negative <- (True <$ char '-') <|> pure False
        hours <- lengthedDecimal 3 <|> lengthedDecimal 2
        (minutes, seconds, microseconds) <- parseMSU
        let delta = hours * 3600 + minutes * 60 + seconds + microseconds * 1e-6
        pure (if negative then negate delta else delta)

instance FromField Value where
  fromField = parseChecking "Aeson.Value" representableBytes (go =<< takeLazyByteString)
    where
      go lbs = case eitherDecode' lbs of
        Left err -> fail err
        Right v  -> pure v

-- Helpers

-- HOF to automate the parsing of many common types
parseChecking ::
  String -> (FieldType -> Bool) -> Parser a -> ColumnDef -> Maybe ByteString -> FieldParser a
parseChecking typeName typePred p f = \case
  Nothing -> throwError ColumnUnexpectedNull
  Just d -> do
    let t = columnType f
    let st = fieldTypeToString t
    if typePred t
      then case parseOnly p d of
        Left err -> throwError (ColumnTypeMismatch typeName st err)
        Right i  -> pure i
      else throwError (ColumnTypeMismatch typeName st "")

-- Checks if a type can be represented in N bits for various n
fits8 :: FieldType -> Bool
fits8 t =
  if | t == mySQLTypeTiny -> True
     | t == mySQLTypeNewDecimal -> True
     | otherwise -> False

fits16 :: FieldType -> Bool
fits16 t =
  if | fits8 t -> True
     | t == mySQLTypeShort -> True
     | otherwise -> False

fits32 :: FieldType -> Bool
fits32 t =
  if | fits16 t -> True
     | t == mySQLTypeInt24 -> True
     | t == mySQLTypeLong -> True
     | otherwise -> False

fits64 :: FieldType -> Bool
fits64 t =
  if | fits32 t -> True
     | t == mySQLTypeLongLong -> True
     | otherwise -> False

-- Checks if a type can fit into a Word on the current platform
fitsWord :: FieldType -> Bool
fitsWord =
  if finiteBitSize (zeroBits :: Word) == 64
    then fits64
    else fits32 -- conservative, but better safe than sorry

-- Checks if we can represent this a a blob of bytes
representableBytes :: FieldType -> Bool
representableBytes t =
  if | representableText t -> True
     | t == mySQLTypeTinyBlob -> True
     | t == mySQLTypeMediumBlob -> True
     | t == mySQLTypeLongBlob -> True
     | otherwise -> False

-- Checks if we can represent this as text
representableText :: FieldType -> Bool
representableText t =
  if | t == mySQLTypeVarChar -> True
     | t == mySQLTypeVarString -> True
     | t == mySQLTypeString -> True
     | t == mySQLTypeEnum -> True
     | t == mySQLTypeBlob -> True -- suspicious
     | otherwise -> False

-- Checks if a type can be represented as a Scientific
representableScientific :: FieldType -> Bool
representableScientific t =
  if | fits64 t -> True
     | t == mySQLTypeFloat -> True
     | t == mySQLTypeDouble -> True
     | t == mySQLTypeDecimal -> True
     | t == mySQLTypeNewDecimal -> True
     | otherwise -> False

parseDayAndTime :: Parser (Day, TimeOfDay)
parseDayAndTime = do
  day <- parseDay
  _ <- char ' '
  time <- parseTime
  pure (day, time)

-- Attempts to parse HH:MM:SS.MMMMMM format timestamp
parseTime :: Parser TimeOfDay
parseTime = do
  hours <- lengthedDecimal 2
  _ <- char ':'
  (minutes, seconds, microseconds) <- parseMSU
  let pico = seconds + microseconds * 1e-6
  case makeTimeOfDayValid hours minutes pico of
    Nothing  -> fail (msg hours minutes seconds microseconds)
    Just tod -> pure tod
  where
    msg h m s u =
      "Invalid time: " <> show h <> ":" <> show m <> ":" <> show s <> "." <> show u

-- Attempts to parse ':MM:SS.MMMMMM'-style segment
parseMSU :: (Num a, Num b, Num c) => Parser (a, b, c)
parseMSU = do
  _ <- char ':'
  m <- lengthedDecimal 2
  _ <- char ':'
  s <- lengthedDecimal 2
  u <- (char '.' *> maxLengthedDecimal 6) <|> pure 0
  pure (m, s, u)

-- Attempts to parse YYYY-MM-DD format datestamp
parseDay :: Parser Day
parseDay = do
  year <- lengthedDecimal 4
  _ <- char '-'
  month <- lengthedDecimal 2
  _ <- char '-'
  day <- lengthedDecimal 2
  case fromGregorianValid year month day of
    Nothing   -> fail (msg year month day)
    Just day' -> pure day'
  where
    msg y m d =
      "Invalid date: " <> show y <> "-" <> show m <> "-" <> show d

-- Parse a decimal with exactly n digits
lengthedDecimal :: (Num a) => Word -> Parser a
lengthedDecimal = go' 0
  where
    go' !x 0 = pure x
    go' !x n = do
      d <- digitToInt <$> digit
      go' (x * 10 + fromIntegral d) (n - 1)

-- Parse a decimal with at most n digits
maxLengthedDecimal :: (Num a) => Word -> Parser a
maxLengthedDecimal = go 0
  where
    go x n = do
      d <- digitToInt <$> digit
      go' (x * 10 + fromIntegral d) (n - 1)
    go' !x 0 = pure x
    go' !x n = go x n <|> pure (x * 10 ^ n)

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
