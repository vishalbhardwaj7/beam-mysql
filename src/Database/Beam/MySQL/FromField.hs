{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Database.Beam.MySQL.FromField
    ( FieldParser
    , FromField(..)) where
    -- , atto ) where

import           Control.Monad.Except              (ExceptT, throwError)
import           Data.Attoparsec.ByteString.Char8  (Parser, decimal, double,
                                                    parseOnly, signed)
import           Data.Bits                         (finiteBitSize, zeroBits)
import           Data.ByteString.Char8             (ByteString, uncons)
import           Data.Int                          (Int16, Int32, Int64, Int8)
import           Data.Word                         (Word16, Word32, Word64,
                                                    Word8)
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

{-
instance FromField Bool where
    fromField f Nothing     = unexpectedNull f ""
    fromField f md@(Just d) = (/= 0) <$>
        case fieldType f of
          Tiny -> fromField f md
          Bit  -> case parseBit d of
            Left err -> conversionFailed f err
            Right r  -> pure r
          _    -> incompatibleTypes f ""
      where
        parseBit :: SB.ByteString -> Either String Word8
        parseBit bs
          | BS.null bs = Left  (   show bs)
          | otherwise  = Right (BS.head bs)

instance FromField Word where
    fromField = atto check64 decimal
instance FromField Word8 where
    fromField = atto check8 decimal
instance FromField Word16 where
    fromField = atto check16 decimal
instance FromField Word32 where
    fromField = atto check32 decimal
instance FromField Word64 where
    fromField = atto check64 decimal

instance FromField Int where
    fromField = atto check64 (signed decimal)
instance FromField Int8 where
    fromField = atto check8 (signed decimal)
instance FromField Int16 where
    fromField = atto check16 (signed decimal)
instance FromField Int32 where
    fromField = atto check32 (signed decimal)
instance FromField Int64 where
    fromField = atto check64 (signed decimal)

instance FromField Float where
    fromField = atto check (realToFrac <$> double)
      where
        check ty         | check16 ty = True
        check Int24      = True
        check Float      = True
        check Decimal    = True
        check NewDecimal = True
        check Double     = True
        check _=         False

instance FromField Double where
    fromField = atto check double
      where
        check ty         | check32 ty = True
        check Float      = True
        check Double     = True
        check Decimal    = True
        check NewDecimal = True
        check _          = False

instance FromField Scientific where
    fromField = atto checkScientific rational

instance FromField (Ratio Integer) where
    fromField = atto checkScientific rational

instance FromField a => FromField (Maybe a) where
    fromField _ Nothing      = pure Nothing
    fromField field (Just d) = Just <$> fromField field (Just d)

instance FromField SqlNull where
    fromField _ Nothing = pure SqlNull
    fromField f _ = throwError (ColumnTypeMismatch "SqlNull"
                                                   (show (fieldType f))
                                                   "Non-null value found")

instance FromField SB.ByteString where
    fromField = doConvert checkBytes pure

instance FromField LB.ByteString where
    fromField f d = fmap (LB.fromChunks . pure) (fromField f d)

instance FromField TS.Text where
    fromField = doConvert checkText (either (Left . show) Right . TE.decodeUtf8')

instance FromField TL.Text where
    fromField f d = fmap (TL.fromChunks . pure) (fromField f d)

instance FromField LocalTime where
    fromField = atto checkDate localTime
      where
        checkDate DateTime  = True
        checkDate Timestamp = True
        checkDate Date      = True
        checkDate _         = False

        localTime = do
          (day, time) <- dayAndTime
          pure (LocalTime day time)

instance FromField Day where
    fromField = atto checkDay dayP
      where
        checkDay Date = True
        checkDay _    = False

instance FromField TimeOfDay where
    fromField = atto checkTime timeP
      where
        checkTime Time = True
        checkTime _    = False

instance FromField NominalDiffTime where
    fromField = atto checkTime durationP
      where
        checkTime Time = True
        checkTime _    = False

instance FromField A.Value where
    fromField f bs =
        case (maybeToRight "Failed to extract JSON bytes." bs) >>= A.eitherDecodeStrict of
            Left err -> conversionFailed f err
            Right x  -> pure x

dayAndTime :: Parser (Day, TimeOfDay)
dayAndTime = do
  day <- dayP
  _ <- char ' '
  time <- timeP

  pure (day, time)

timeP :: Parser TimeOfDay
timeP = do
  hour <- lengthedDecimal 2
  _ <- char ':'
  minute <- lengthedDecimal 2
  _ <- char ':'
  seconds <- lengthedDecimal 2
  microseconds <- (char '.' *> maxLengthedDecimal 6) <|>
                  pure 0

  let pico = seconds + microseconds * 1e-6
  case makeTimeOfDayValid hour minute pico of
    Nothing -> fail (printf "Invalid time part: %02d:%02d:%s" hour minute (showFixed False pico))
    Just tod -> pure tod

durationP :: Parser NominalDiffTime
durationP = do
  negative <- (True <$ char '-') <|> pure False
  hour <- lengthedDecimal 3
  _ <- char ':'
  minute <- lengthedDecimal 2
  _ <- char ':'
  seconds <- lengthedDecimal 2
  microseconds <- (char '.' *> maxLengthedDecimal 6) <|>
                  pure 0

  let v = hour * 3600 + minute * 60 + seconds +
          microseconds * 1e-6

  pure (if negative then negate v else v)

dayP :: Parser Day
dayP = do
  year <- lengthedDecimal 4
  _ <- char '-'
  month <- lengthedDecimal 2
  _ <- char '-'
  day <- lengthedDecimal 2

  case fromGregorianValid year month day of
    Nothing -> fail (printf "Invalid date part: %04d-%02d-%02d" year month day)
    Just day' -> pure day'

lengthedDecimal :: Num a => Int -> Parser a
lengthedDecimal = lengthedDecimal' 0
  where
    lengthedDecimal' !a 0 = pure a
    lengthedDecimal' !a n = do
      d <- digitToInt <$> digit
      lengthedDecimal' (a * 10 + fromIntegral d) (n - 1)

maxLengthedDecimal :: Num a => Int -> Parser a
maxLengthedDecimal = go1 0
  where
    go1 a n = do
      d <- digitToInt <$> digit
      go' (a * 10 + fromIntegral d) (n - 1)

    go' !a 0 = pure a
    go' !a n =
      go1 a n <|> pure (a * 10  ^ n)

incompatibleTypes, unexpectedNull, conversionFailed
    :: forall a. Typeable a => Field -> String -> FieldParser a
incompatibleTypes f msg =
  throwError (ColumnTypeMismatch (show (typeRep (Proxy :: Proxy a)))
                                 (show (fieldType f))
                                 msg)
unexpectedNull _ _ =
  throwError ColumnUnexpectedNull
conversionFailed f msg =
  throwError (ColumnTypeMismatch (show (typeRep (Proxy :: Proxy a)))
                                 (show (fieldType f))
                                 msg)

check8, check16, check32, check64, checkScientific, checkBytes, checkText
    :: Type -> Bool

check8 Tiny       = True
check8 NewDecimal = True
check8 _          = False

check16 ty    | check8 ty = True
check16 Short = True
check16 _=    False

check32 ty    | check16 ty = True
check32 Int24 = True
check32 Long  = True
check32 _     = False

check64 ty       | check32 ty = True
check64 LongLong = True
check64 _        = False

checkScientific ty         | check64 ty = True
checkScientific Float      = True
checkScientific Double     = True
checkScientific Decimal    = True
checkScientific NewDecimal = True
checkScientific _          = True

checkBytes ty         | checkText ty = True
checkBytes TinyBlob   = True
checkBytes MediumBlob = True
checkBytes LongBlob   = True
checkBytes Blob       = True
checkBytes _          = False

checkText VarChar   = True
checkText VarString = True
checkText String    = True
checkText Enum      = True
checkText Blob      = True
checkText _         = False

doConvert :: Typeable a => (Type -> Bool)
          -> (SB.ByteString -> Either String a)
          -> Field -> Maybe SB.ByteString -> FieldParser a
doConvert _ _ f Nothing = unexpectedNull f ""
doConvert checkType parser field (Just d)
  | checkType (fieldType field) =
      case parser d of
        Left err -> conversionFailed field err
        Right r  -> pure r
  | otherwise = incompatibleTypes field ""

atto :: Typeable a => (Type -> Bool) -> Parser a
     -> Field -> Maybe SB.ByteString -> FieldParser a
atto checkType parser =
  doConvert checkType (parseOnly parser)

maybeToRight :: b -> Maybe a -> Either b a
maybeToRight _ (Just x) = Right x
maybeToRight y Nothing  = Left y
-}
