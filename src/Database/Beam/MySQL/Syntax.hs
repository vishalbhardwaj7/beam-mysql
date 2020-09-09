module Database.Beam.MySQL.Syntax where


{-
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Database.Beam.MySQL.Syntax
(
  MysqlSyntax (..),
  MysqlTableNameSyntax (..),
  MysqlInsertValuesSyntax (..),
  MysqlInsertSyntax (..),
  intoQuery, intoDebugText, intoTableName, bracketWrap,
  defaultE, backtickWrap, intoLazyText, textSyntax, quoteWrapUnescaped
) where

-- TODO: Use qualified imports
import           Data.Aeson (Value)
import           Data.Aeson.Text (encodeToTextBuilder)
import           Data.ByteString (ByteString)
import           Data.Fixed (E12, Fixed, showFixed)
import           Data.Foldable (fold)
import           Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import           Data.Int (Int16, Int32, Int64, Int8)
import           Data.List (intersperse)
import           Data.Scientific (Scientific)
import           Data.String (IsString, fromString)
import           Data.Text (Text)
import qualified Data.Text.Lazy as TL
import           Data.Text.Lazy.Builder (Builder, fromText, toLazyText)
import           Data.Text.Lazy.Builder.Scientific (scientificBuilder)
import qualified Data.Text.Lazy.Encoding as TLE
import           Data.Time.Calendar (Day)
import           Data.Time.Clock (NominalDiffTime)
import           Data.Time.Format.ISO8601 (formatShow, iso8601Format)
import           Data.Time.LocalTime (LocalTime, TimeOfDay, localDay,
                                      localTimeOfDay)
import           Data.Word (Word16, Word32, Word64, Word8)
import           Database.Beam.Backend.SQL (HasSqlValueSyntax (..), IsSql92AggregationExpressionSyntax (..),
                                            IsSql92AggregationSetQuantifierSyntax (..),
                                            IsSql92DataTypeSyntax (..),
                                            IsSql92DeleteSyntax (..),
                                            IsSql92ExpressionSyntax (..),
                                            IsSql92ExtractFieldSyntax (..),
                                            IsSql92FieldNameSyntax (..),
                                            IsSql92FromSyntax (..),
                                            IsSql92GroupingSyntax (..),
                                            IsSql92InsertSyntax (..),
                                            IsSql92InsertValuesSyntax (..),
                                            IsSql92OrderingSyntax (..),
                                            IsSql92ProjectionSyntax (..),
                                            IsSql92QuantifierSyntax (..),
                                            IsSql92SelectSyntax (..),
                                            IsSql92SelectTableSyntax (..),
                                            IsSql92Syntax (..),
                                            IsSql92TableNameSyntax (..),
                                            IsSql92TableSourceSyntax (..),
                                            IsSql92UpdateSyntax (..),
                                            IsSql99ConcatExpressionSyntax (..),
                                            SqlNull (..))
import           Database.MySQL.Base (Query (..))
import qualified Database.MySQL.Protocol.Escape as Escape
import           Fmt (build, unlinesF, whenF, (+|), (|+))

-- General syntax type
newtype MysqlSyntax = MysqlSyntax (HashSet Text, Builder)
  deriving newtype (Semigroup, Monoid, Eq)

instance IsString MysqlSyntax where
 fromString = MysqlSyntax . (HS.empty,) . fromString

intoQuery :: MysqlSyntax -> Query
intoQuery (MysqlSyntax (_, b)) = Query . TLE.encodeUtf8 . toLazyText $ b

intoDebugText :: MysqlSyntax -> Text
intoDebugText (MysqlSyntax (tables, b)) =
  "Query: " +|
  toLazyText b |+
  ", relevant table(s):" +|
  unlinesF tables |+
  ""

-- SAFE: unwrap of safe values (by induction)
intoLazyText :: MysqlSyntax -> TL.Text
intoLazyText (MysqlSyntax (_, b)) = toLazyText b

-- SAFE: Word -> Text
charLen :: Maybe Word -> MysqlSyntax
charLen =
  MysqlSyntax . (HS.empty,) . maybe "MAX" (fromString . show)

-- POTENTIALLY UNSAFE:
--   charset comes from `beam`, but what are these values and where do
--   they come from?
charSet :: Maybe Text -> MysqlSyntax
charSet = MysqlSyntax . (HS.empty,) . foldMap go
  where
    go :: Text -> Builder
    go = (" CHARACTER SET " <>) . backtickWrap . fromText

-- SAFE: Word -> Text
numPrec :: Maybe (Word, Maybe Word) -> MysqlSyntax
numPrec = \case
  Nothing -> mempty
  Just (d, mn) -> MysqlSyntax . (HS.empty,) . bracketWrap $ case mn of
    Nothing -> fromString . show $ d
    Just n  -> (fromString . show $ d) <> ", " <> (fromString . show $ n)

-- SAFE iff inputs are safe
textSyntax :: Text -> MysqlSyntax
textSyntax = MysqlSyntax . (HS.empty,) . fromText

-- Combination of data type name and its casting
data MysqlDataTypeSyntax = MysqlDataTypeSyntax {
  _mysqlDataType :: !MysqlSyntax,
  mysqlTypeCast  :: !MysqlSyntax
  }
  deriving stock (Eq)

-- Table name with optional database name
data MysqlTableNameSyntax = MysqlTableNameSyntax
  !(Maybe Text)
  {-# UNPACK #-} !Text
  deriving stock (Eq)

-- SAFE given that beam tables are statically defined
intoTableName :: MysqlTableNameSyntax -> MysqlSyntax
intoTableName (MysqlTableNameSyntax db name) =
  MysqlSyntax . (HS.singleton name,) $
    foldMap go db <> (backtickWrap . fromText $ name)
  where
    go :: Text -> Builder
    go t = (backtickWrap . fromText $ t) <> "."

-- Insert values syntax to support runInsertRowReturning
data MysqlInsertValuesSyntax =
  FromExprs [[MysqlSyntax]] |
  FromSQL MysqlSyntax
  deriving stock (Eq)

-- Insert syntax to support runInsertRowReturning
data MysqlInsertSyntax =
  Insert
    { mysqlTableName :: MysqlTableNameSyntax
    , fieldNames     :: [Text]
    , fieldValues    :: MysqlInsertValuesSyntax
    }
  deriving stock (Eq)

-- How we convert everything types defined in FromField to MySQL syntax

-- SAFE: constants
instance HasSqlValueSyntax MysqlSyntax Bool where
  sqlValueSyntax = \case
    True -> "TRUE"
    False -> "FALSE"

-- SAFE: Int -> Text
instance HasSqlValueSyntax MysqlSyntax Int8 where
  sqlValueSyntax = fromString . show

-- SAFE: Int -> Text
instance HasSqlValueSyntax MysqlSyntax Int16 where
  sqlValueSyntax = fromString . show

-- SAFE: Int -> Text
instance HasSqlValueSyntax MysqlSyntax Int32 where
  sqlValueSyntax = fromString . show

-- SAFE: Int -> Text
instance HasSqlValueSyntax MysqlSyntax Int64 where
  sqlValueSyntax = fromString . show

-- SAFE: Int -> Text
instance HasSqlValueSyntax MysqlSyntax Int where
  sqlValueSyntax = fromString . show

-- SAFE: Int -> Text
instance HasSqlValueSyntax MysqlSyntax Word8 where
  sqlValueSyntax = fromString . show

-- SAFE: Int -> Text
instance HasSqlValueSyntax MysqlSyntax Word16 where
  sqlValueSyntax = fromString . show

-- SAFE: Int -> Text
instance HasSqlValueSyntax MysqlSyntax Word32 where
  sqlValueSyntax = fromString . show

-- SAFE: Int -> Text
instance HasSqlValueSyntax MysqlSyntax Word64 where
  sqlValueSyntax = fromString . show

-- SAFE: Int -> Text
instance HasSqlValueSyntax MysqlSyntax Word where
  sqlValueSyntax = fromString . show

{- SAFE: escaped

Notes: a plain `show` *should* be safe:

  Prelude Numeric.IEEE> infinity :: Double
  Infinity
  Prelude Numeric.IEEE> nan :: Double
  NaN

However, escape to be sure
-}
instance HasSqlValueSyntax MysqlSyntax Float where
  sqlValueSyntax = textSyntax . Escape.escapeText . fromString . show

{- SAFE: escaped -}
instance HasSqlValueSyntax MysqlSyntax Double where
  sqlValueSyntax = textSyntax . Escape.escapeText . fromString . show

-- SAFE: escaped
instance HasSqlValueSyntax MysqlSyntax Scientific where
  sqlValueSyntax = textSyntax . Escape.escapeText . builderToText . scientificBuilder

-- Rational is exempted, because there's no good way to do this really

-- SAFE: by induction
instance (HasSqlValueSyntax MysqlSyntax a) =>
  HasSqlValueSyntax MysqlSyntax (Maybe a) where
  sqlValueSyntax = maybe (sqlValueSyntax SqlNull) sqlValueSyntax

-- SAFE: constant
instance HasSqlValueSyntax MysqlSyntax SqlNull where
  sqlValueSyntax = const "NULL"

-- DISABLED
instance HasSqlValueSyntax MysqlSyntax ByteString where
  -- TODO: It is *probably* correct to escape the bytestring, decode with
  --       latin1 and encode with latin1 to go from Text -> ByteString
  --       in `intoQuery`; disable for now
  sqlValueSyntax = error "Dabase.Beam.MySQL: ByteString not supported"
  -- textSyntax . quoteWrapUnescaped . decodeUtf8 . Escape.escapeBytes

-- SAFE: escaped
instance HasSqlValueSyntax MysqlSyntax Text where
  sqlValueSyntax = quote

-- SAFE: escaped
instance HasSqlValueSyntax MysqlSyntax Day where
  sqlValueSyntax = quote . fromString . formatShow iso8601Format

-- SAFE: escaped
instance HasSqlValueSyntax MysqlSyntax TimeOfDay where
  sqlValueSyntax = quote . fromString . formatShow iso8601Format

-- SAFE: escaped
instance HasSqlValueSyntax MysqlSyntax LocalTime where
  sqlValueSyntax lt = quote (day <> " " <> tod)
    where
      day = fromString . formatShow iso8601Format . localDay $ lt
      tod = fromString . formatShow iso8601Format . localTimeOfDay $ lt

{- SAFE: escaped

NOTE: this is probably safe without escaping, but depends on the formatting
      internals of a third-party library; we do not want to risk this
-}
instance HasSqlValueSyntax MysqlSyntax NominalDiffTime where
  sqlValueSyntax d = textSyntax $ Escape.escapeText time
    where
      dWhole :: Int
      dWhole = abs . floor $ d
      hours :: Int
      hours = dWhole `div` 3600
      d' :: Int
      d' = dWhole - (hours * 3600)
      minutes :: Int
      minutes = d' `div` 60
      seconds :: NominalDiffTime
      seconds = abs d - fromIntegral ((hours * 3600) + (minutes * 60))
      secondsFixed :: Fixed E12
      secondsFixed = fromRational . toRational $ seconds
      time :: Text
      time = whenF (d < 0) (build @Text "-") +|
              whenF (hours < 10) (build @Text "0") +|
              hours |+
              ":" +|
              whenF (minutes < 10) (build @Text "0") +|
              minutes |+
              ":" +|
              whenF (secondsFixed < 10) (build @Text "0") +|
              showFixed False secondsFixed |+
              ""

-- SAFE depending on inputs
builderToText :: Builder -> Text
builderToText = TL.toStrict . toLazyText

-- SAFE: escaped
instance HasSqlValueSyntax MysqlSyntax Value where
  -- TODO: Aeson should return a Text
  sqlValueSyntax = quote . builderToText . encodeToTextBuilder

-- Extra convenience instances

-- SAFE: Int -> Text
instance HasSqlValueSyntax MysqlSyntax Integer where
  sqlValueSyntax = fromString . show

-- SAFE: escaped
instance HasSqlValueSyntax MysqlSyntax String where
  sqlValueSyntax = quote . fromString

-- SAFE: escaped
instance HasSqlValueSyntax MysqlSyntax TL.Text where
  sqlValueSyntax = quote . TL.toStrict

-- Syntax defs

-- SAFE: beam fields are static
instance IsSql92FieldNameSyntax MysqlSyntax where
  unqualifiedField = backtickWrap . textSyntax
  qualifiedField qual f =
    (backtickWrap . textSyntax $ qual) <>
    "." <>
    unqualifiedField f

-- SAFE: constants
instance IsSql92QuantifierSyntax MysqlSyntax where
  quantifyOverAll = "ALL"
  quantifyOverAny = "ANY"

-- SAFE (probably): depends on values from `beam` and constants
--
-- TODO: Figure out what values beam provides here; needs review
--
instance IsSql92DataTypeSyntax MysqlDataTypeSyntax where
  domainType t = MysqlDataTypeSyntax go go
    where
      go = backtickWrap . textSyntax $ t
  charType mlen mcs = MysqlDataTypeSyntax def "CHAR"
    where
      def = "CHAR" <> charLen mlen <> charSet mcs
  varCharType mlen mcs = MysqlDataTypeSyntax def "CHAR"
    where
      def = "VARCHAR" <> charLen mlen <> charSet mcs
  nationalCharType mlen = MysqlDataTypeSyntax def "CHAR"
    where
      def = "NATIONAL CHAR" <> charLen mlen
  nationalVarCharType mlen = MysqlDataTypeSyntax def "CHAR"
    where
      def = "NATIONAL CHAR VARYING" <> charLen mlen
  bitType mlen = MysqlDataTypeSyntax def "BINARY"
    where
      def = "BIT" <> charLen mlen
  varBitType mlen = MysqlDataTypeSyntax def "BINARY"
    where
      def = "VARBINARY" <> charLen mlen
  numericType mprec =
    MysqlDataTypeSyntax ("NUMERIC" <> prec) ("DECIMAL" <> prec)
    where
      prec = numPrec mprec
  decimalType mprec = MysqlDataTypeSyntax go go
    where
      go = "DECIMAL" <> numPrec mprec
  intType = MysqlDataTypeSyntax "INT" "INTEGER"
  smallIntType = MysqlDataTypeSyntax "SMALL INT" "INTEGER"
  floatType mprec = MysqlDataTypeSyntax def "DECIMAL"
    where
      def = MysqlSyntax . (HS.empty,) $
        "FLOAT" <> foldMap (bracketWrap . fromString . show) mprec
  doubleType = MysqlDataTypeSyntax "DOUBLE" "DECIMAL"
  realType = MysqlDataTypeSyntax "REAL" "DECIMAL"
  dateType = MysqlDataTypeSyntax "DATE" "DATE"
  timeType _ _ = MysqlDataTypeSyntax "TIME" "TIME"
  timestampType _ _ = MysqlDataTypeSyntax "TIMESTAMP" "DATETIME"

-- Safe: constants
instance IsSql92ExtractFieldSyntax MysqlSyntax where
  secondsField = "SECOND"
  minutesField = "MINUTE"
  hourField = "HOUR"
  dayField = "DAY"
  monthField = "MONTH"
  yearField = "YEAR"

-- SAFE: all these functions are SAFE, which follows from constants and
-- by induction, as all functions are `expr -> expr`
instance IsSql92ExpressionSyntax MysqlSyntax where
  type Sql92ExpressionValueSyntax MysqlSyntax = MysqlSyntax
  type Sql92ExpressionFieldNameSyntax MysqlSyntax = MysqlSyntax
  type Sql92ExpressionQuantifierSyntax MysqlSyntax = MysqlSyntax
  type Sql92ExpressionCastTargetSyntax MysqlSyntax = MysqlDataTypeSyntax
  type Sql92ExpressionExtractFieldSyntax MysqlSyntax = MysqlSyntax
  type Sql92ExpressionSelectSyntax MysqlSyntax = MysqlSyntax
  addE = binOp "+"
  subE = binOp "-"
  mulE = binOp "*"
  divE = binOp "/"
  modE = binOp "%"
  orE = binOp "OR"
  andE = binOp "AND"
  -- TODO: Need escaping of _ and % in LIKE patterns
  likeE = error "Database.Beam.MySQL: LIKE not supported" -- binOp "LIKE"
  overlapsE = binOp "OVERLAPS"
  eqE = compOp "="
  neqE = compOp "<>"
  ltE = compOp "<"
  gtE = compOp ">"
  leE = compOp "<="
  geE = compOp ">="
  negateE = unaryOp "-"
  notE = unaryOp "NOT"
  existsE = funcall "EXISTS"
  uniqueE = funcall "UNIQUE"
  isNotNullE = postfix "IS NOT NULL"
  isNullE = postfix "IS NULL"
  isTrueE = postfix "IS TRUE"
  isFalseE = postfix "IS FALSE"
  isNotTrueE = postfix "IS NOT TRUE"
  isNotFalseE = postfix "IS NOT FALSE"
  isUnknownE = postfix "IS UNKNOWN"
  isNotUnknownE = postfix "IS NOT UNKNOWN"
  betweenE arg lo hi = bracketWrap arg <>
                        " BETWEEN " <>
                        bracketWrap lo <>
                        " AND " <>
                        bracketWrap hi
  valueE = id
  rowE = bracketWrap . fold . intersperse ", "
  fieldE = id
  subqueryE = bracketWrap
  positionE needle haystack =
    "POSITION" <> bracketWrap (bracketWrap needle <> " IN " <> bracketWrap haystack)
  nullIfE l r = "NULLIF" <> bracketWrap (l <> ", " <> r)
  absE = funcall "ABS"
  bitLengthE = funcall "BIT_LENGTH"
  charLengthE = funcall "CHAR_LENGTH"
  octetLengthE = funcall "OCTET_LENGTH"
  coalesceE = ("COALESCE" <>) . bracketWrap . fold . intersperse ", "
  extractE field from =
    "EXTRACT" <> bracketWrap (field <> " FROM " <> bracketWrap from)
  castE e to =
    "CAST" <>
      bracketWrap (bracketWrap e <> " AS " <> mysqlTypeCast to)
  caseE cases def =
    "CASE " <> foldMap go cases <> "ELSE " <> def <> " END"
    where go (cond, res) = "WHEN " <> cond <> " THEN " <> res <> " "
  currentTimestampE = "CURRENT_TIMESTAMP"
  defaultE = "DEFAULT"
  inE e es =
    bracketWrap e <> " IN " <> bracketWrap go
    where
      go = fold . intersperse ", " $ es
  trimE = funcall "TRIM"
  lowerE = funcall "LOWER"
  upperE = funcall "UPPER"

-- SAFE: constants
instance IsSql92AggregationSetQuantifierSyntax MysqlSyntax where
  setQuantifierDistinct = "DISTINCT"
  setQuantifierAll = "ALL"

-- SAFE: constants and by induction (expr -> expr)
instance IsSql92AggregationExpressionSyntax MysqlSyntax where
  type Sql92AggregationSetQuantifierSyntax MysqlSyntax = MysqlSyntax
  countAllE = "COUNT(*)"
  countE = unaryAggregation "COUNT"
  avgE = unaryAggregation "AVG"
  sumE = unaryAggregation "SUM"
  minE = unaryAggregation "MIN"
  maxE = unaryAggregation "MAX"

-- SAFE: names are statically defined
instance IsSql92ProjectionSyntax MysqlSyntax where
  type Sql92ProjectionExpressionSyntax MysqlSyntax = MysqlSyntax
  projExprs = fold . intersperse ", " . fmap go
    where
      go :: (MysqlSyntax, Maybe Text) -> MysqlSyntax
      go (expr, name) = expr <> foldMap go2 name
      go2 :: Text -> MysqlSyntax
      go2 = (" AS " <>) . backtickWrap . textSyntax

-- SAFE: table names are statically defined
instance IsSql92TableNameSyntax MysqlTableNameSyntax where
  tableName = MysqlTableNameSyntax

instance IsSql92TableSourceSyntax MysqlSyntax where
  type Sql92TableSourceTableNameSyntax MysqlSyntax = MysqlTableNameSyntax
  type Sql92TableSourceSelectSyntax MysqlSyntax = MysqlSyntax
  type Sql92TableSourceExpressionSyntax MysqlSyntax = MysqlSyntax
  tableNamed = intoTableName
  tableFromSubSelect = bracketWrap
  tableFromValues =
    bracketWrap . fold . intersperse " UNION " . fmap (\vals -> "SELECT " <> go vals)
    where
      go :: [MysqlSyntax] -> MysqlSyntax
      go = fold . intersperse ", " . fmap bracketWrap

-- SAFE: by induction on sub-clauses
instance IsSql92FromSyntax MysqlSyntax where
  type Sql92FromTableSourceSyntax MysqlSyntax = MysqlSyntax
  type Sql92FromExpressionSyntax MysqlSyntax = MysqlSyntax
  fromTable tableE = \case
    -- SAFE: by induction
    Nothing -> tableE
    -- SAFE: column names are statically defined, beam temporary names are safe
    Just (name, mCols) ->
      tableE <>
      " AS " <>
      (backtickWrap . textSyntax $ name) <>
      foldMap go mCols
    where
      go :: [Text] -> MysqlSyntax
      go =
        bracketWrap . fold . intersperse ", " . fmap (backtickWrap . textSyntax)
  -- SAEF: induction, constants
  innerJoin = joinOp "JOIN"
  leftJoin = joinOp "LEFT JOIN"
  rightJoin = joinOp "RIGHT JOIN"

-- SAFE: by induction
instance IsSql92GroupingSyntax MysqlSyntax where
  type Sql92GroupingExpressionSyntax MysqlSyntax = MysqlSyntax
  groupByExpressions :: [MysqlSyntax] -> MysqlSyntax
  groupByExpressions = fold . intersperse ", "

-- SAFE: by induction
instance IsSql92SelectTableSyntax MysqlSyntax where
  type Sql92SelectTableSelectSyntax MysqlSyntax = MysqlSyntax
  type Sql92SelectTableExpressionSyntax MysqlSyntax = MysqlSyntax
  type Sql92SelectTableProjectionSyntax MysqlSyntax = MysqlSyntax
  type Sql92SelectTableFromSyntax MysqlSyntax = MysqlSyntax
  type Sql92SelectTableGroupingSyntax MysqlSyntax = MysqlSyntax
  type Sql92SelectTableSetQuantifierSyntax MysqlSyntax = MysqlSyntax

  selectTableStmt
    -- :: Maybe MysqlSyntax
    :: Maybe MysqlSyntax
    -> MysqlSyntax
    -> Maybe MysqlSyntax
    -> Maybe MysqlSyntax
    -> Maybe MysqlSyntax
    -> Maybe MysqlSyntax
    -> MysqlSyntax
  selectTableStmt quant proj from wher grouping having =
    "SELECT " <>
    foldMap (<> " ") quant <>
    proj <>
    foldMap (" FROM " <>) from <>
    foldMap (" WHERE " <>) wher <>
    foldMap (" GROUP BY " <>) grouping <>
    foldMap (" HAVING " <>) having

  unionTables
    :: Bool
    -> MysqlSyntax
    -> MysqlSyntax
    -> MysqlSyntax
  unionTables isAll = if isAll
    then tableOp "UNION ALL"
    else tableOp "UNION"

  intersectTables
    :: Bool
    -> MysqlSyntax
    -> MysqlSyntax
    -> MysqlSyntax
  intersectTables _ = tableOp "INTERSECT"

  exceptTable
    :: Bool
    -> MysqlSyntax
    -> MysqlSyntax
    -> MysqlSyntax
  exceptTable _ = tableOp "EXCEPT"

-- SAFE: by induction
instance IsSql92OrderingSyntax MysqlSyntax where
  type Sql92OrderingExpressionSyntax MysqlSyntax = MysqlSyntax
  ascOrdering :: MysqlSyntax -> MysqlSyntax
  ascOrdering = (<> " ASC")
  descOrdering :: MysqlSyntax -> MysqlSyntax
  descOrdering = (<> " DESC")

-- SAFE: integers, constants, induction
instance IsSql92SelectSyntax MysqlSyntax where
  type Sql92SelectSelectTableSyntax MysqlSyntax = MysqlSyntax
  type Sql92SelectOrderingSyntax MysqlSyntax = MysqlSyntax

  selectStmt
    :: MysqlSyntax
    -> [MysqlSyntax]
    -> Maybe Integer
    -> Maybe Integer
    -> MysqlSyntax
  selectStmt tbl ordering limit offset =
    tbl <>
    (case ordering of
      []      -> mempty
      clauses -> " ORDER BY " <> (fold . intersperse ", " $ clauses)) <>
    case (limit, offset) of
      (Just limit', Just offset') ->
        " LIMIT " <>
        (fromString . show $ offset') <>
        ", " <>
        (fromString . show $ limit')
      (Just limit', Nothing)      -> " LIMIT " <> (fromString . show $ limit')
      _                           -> mempty

-- SAFE: by induction
instance IsSql92InsertValuesSyntax MysqlInsertValuesSyntax where
  type Sql92InsertValuesExpressionSyntax MysqlInsertValuesSyntax = MysqlSyntax
  type Sql92InsertValuesSelectSyntax MysqlInsertValuesSyntax = MysqlSyntax
  insertSqlExpressions = FromExprs
  insertFromSql = FromSQL

-- SAFE: by induction
instance IsSql92InsertSyntax MysqlInsertSyntax where
  type Sql92InsertValuesSyntax MysqlInsertSyntax = MysqlInsertValuesSyntax
  type Sql92InsertTableNameSyntax MysqlInsertSyntax = MysqlTableNameSyntax
  insertStmt = Insert

-- SAFE: by induction
instance IsSql92UpdateSyntax MysqlSyntax where
  type Sql92UpdateExpressionSyntax MysqlSyntax = MysqlSyntax
  type Sql92UpdateFieldNameSyntax MysqlSyntax = MysqlSyntax
  type Sql92UpdateTableNameSyntax MysqlSyntax = MysqlTableNameSyntax
  updateStmt tblName fields wher =
    "UPDATE " <>
    intoTableName tblName <>
    (case fields of
      [] -> mempty
      _  -> " SET " <> (fold . intersperse ", " . fmap go $ fields)) <>
    foldMap (" WHERE " <>) wher
    where go (field, val) = field <> "=" <> val

-- SAFE: by induction
instance IsSql92DeleteSyntax MysqlSyntax where
  type Sql92DeleteTableNameSyntax MysqlSyntax = MysqlTableNameSyntax
  type Sql92DeleteExpressionSyntax MysqlSyntax = MysqlSyntax
  deleteStmt tblName _ wher =
    "DELETE FROM " <>
    intoTableName tblName <>
    foldMap (" WHERE " <>) wher
  deleteSupportsAlias = const False

-- SAFE: by induction
instance IsSql92Syntax MysqlSyntax where
  type Sql92SelectSyntax MysqlSyntax = MysqlSyntax
  type Sql92InsertSyntax MysqlSyntax = MysqlInsertSyntax
  type Sql92UpdateSyntax MysqlSyntax = MysqlSyntax
  type Sql92DeleteSyntax MysqlSyntax = MysqlSyntax
  selectCmd = id
  insertCmd (Insert tblName fields values) =
    "INSERT INTO " <>
    intoTableName tblName <>
    (bracketWrap . fold . intersperse ", " . fmap textSyntax $ fields) <>
    go values
    where
      go :: MysqlInsertValuesSyntax -> MysqlSyntax
      go = \case
        FromExprs exprs ->
          " VALUES " <>
          (fold . intersperse ", " . fmap go2 $ exprs)
        FromSQL sql -> sql
      go2 :: [MysqlSyntax] -> MysqlSyntax
      go2 = bracketWrap . fold . intersperse ", "
  updateCmd = id
  deleteCmd = id

instance IsSql99ConcatExpressionSyntax MysqlSyntax where
  concatE = \case
    [] -> mempty
    xs -> "CONCAT(" <> (fold . intersperse ", " $ xs) <> ")"

-- Helpers

-- SAFE: escaped
quote :: Text -> MysqlSyntax
quote = textSyntax . quoteWrap

-- SAFE: escaped
quoteWrap :: Text -> Text
quoteWrap = wrap "'" "'" . Escape.escapeText

-- SAFE: depending on inputs
quoteWrapUnescaped :: (IsString s, Semigroup s) => s -> s
quoteWrapUnescaped = wrap "'" "'"

-- SAFE: depending on inputs
backtickWrap :: (IsString s, Semigroup s) => s -> s
backtickWrap = wrap "`" "`"

-- SAFE: depending on inputs
bracketWrap :: (IsString s, Semigroup s) => s -> s
bracketWrap = wrap "(" ")"

-- SAFE: depending on inputs
wrap :: (Semigroup s) => s -> s -> s -> s
wrap lDelim rDelim x = lDelim <> x <> rDelim

-- SAFE: depending on inputs
binOp :: (Semigroup s, IsString s) => s -> s -> s -> s
binOp op lOp rOp = bracketWrap lOp <> " " <> op <> " " <> bracketWrap rOp

-- SAFE: depending on inputs
compOp :: (IsString s, Foldable t, Monoid s) => s -> t s -> s -> s -> s
compOp op quantifier lOp rOp =
  bracketWrap lOp <> " " <> op <> fold quantifier <> " " <> bracketWrap rOp

-- SAFE: depending on inputs
unaryOp :: (Semigroup s, IsString s) => s -> s -> s
unaryOp op arg = op <> bracketWrap arg <> " "

-- SAFE: depending on inputs
postfix :: (Semigroup s, IsString s) => s -> s -> s
postfix op arg = bracketWrap arg <> " " <> op

-- SAFE: depending on inputs
funcall :: (Semigroup s, IsString s) => s -> s -> s
funcall fn arg = fn <> bracketWrap arg

-- SAFE: depending on inputs
unaryAggregation :: (IsString s, Foldable t, Monoid s) => s -> t s -> s -> s
unaryAggregation fn q e = fn <> bracketWrap (foldMap (<> " ") q <> e)

-- SAFE: depending on inputs
joinOp :: (IsString s, Foldable t, Monoid s) =>
  s -> s -> s -> t s -> s
joinOp joinType l r mOn = l <> " " <> joinType <> " " <> r <> fold mOn

-- SAFE: depending on inputs
tableOp :: (Semigroup s, IsString s) => s -> s -> s -> s
tableOp op l r = l <> " " <> op <> " " <> r -}
