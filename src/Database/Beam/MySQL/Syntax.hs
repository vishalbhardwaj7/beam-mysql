{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Database.Beam.MySQL.Syntax
(
  MysqlSyntax (..),
  MysqlTableNameSyntax (..),
  MysqlInsertValuesSyntax (..),
  MysqlInsertSyntax (..),
  intoQuery, intoDebugText, intoTableName, bracketWrap
) where

import           Data.Aeson (Value)
import           Data.Aeson.Text (encodeToTextBuilder)
import           Data.ByteString (ByteString)
import           Data.Fixed (E12, Fixed, showFixed)
import           Data.Foldable (fold)
import           Data.Int (Int16, Int32, Int64, Int8)
import           Data.List (intersperse)
import           Data.Scientific (Scientific)
import           Data.String (IsString, fromString)
import           Data.Text (Text)
import           Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.Lazy as TL
import           Data.Text.Lazy.Builder (Builder, fromLazyText, fromText,
                                         toLazyText)
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

-- General syntax type
newtype MysqlSyntax = MysqlSyntax Builder
  deriving newtype (Semigroup, Monoid, Eq, IsString)

intoQuery :: MysqlSyntax -> Query
intoQuery (MysqlSyntax b) = Query . TLE.encodeUtf8 . toLazyText $ b

intoDebugText :: MysqlSyntax -> Text
intoDebugText (MysqlSyntax b) = TL.toStrict . toLazyText $ b

quoteWrap :: Builder -> MysqlSyntax
quoteWrap = wrap "'" "'"

backtickWrap :: Builder -> MysqlSyntax
backtickWrap = wrap "`" "`"

bracketWrap :: Builder -> MysqlSyntax
bracketWrap = wrap "(" ")"

wrap :: Builder -> Builder -> Builder -> MysqlSyntax
wrap lDelim rDelim b = MysqlSyntax (lDelim <> b <> rDelim)

charLen :: Maybe Word -> MysqlSyntax
charLen = bracketWrap . maybe "MAX" (fromString . show)

charSet :: Maybe Text -> MysqlSyntax
charSet = foldMap ((" CHARACTER SET " <>) . backtickWrap . fromText)

numPrec :: Maybe (Word, Maybe Word) -> MysqlSyntax
numPrec = \case
  Nothing -> mempty
  Just (d, Nothing) -> bracketWrap . fromString . show $ d
  Just (d, Just n) -> bracketWrap ((fromString . show $ d) <> ", " <> (fromString . show $ n))

unaryOp :: MysqlSyntax -> MysqlSyntax -> MysqlSyntax
unaryOp op (MysqlSyntax arg) = op <> bracketWrap arg <> " "

unaryAggregation :: MysqlSyntax -> Maybe MysqlSyntax -> MysqlSyntax -> MysqlSyntax
unaryAggregation fn q (MysqlSyntax e) = fn <> bracketWrap (go <> e)
  where MysqlSyntax go = foldMap (<> " ") q

binOp :: MysqlSyntax -> MysqlSyntax -> MysqlSyntax -> MysqlSyntax
binOp op (MysqlSyntax l) (MysqlSyntax r) =
  bracketWrap l <> " " <> op <> " " <> bracketWrap r

compOp :: MysqlSyntax -> Maybe MysqlSyntax -> MysqlSyntax -> MysqlSyntax -> MysqlSyntax
compOp op quantifier (MysqlSyntax l) (MysqlSyntax r) =
  bracketWrap l <> " " <> op <> fold quantifier <> " " <> bracketWrap r

postfix :: MysqlSyntax -> MysqlSyntax -> MysqlSyntax
postfix op (MysqlSyntax e) = bracketWrap e <> " " <> op

joinOp :: MysqlSyntax -> MysqlSyntax -> MysqlSyntax -> Maybe MysqlSyntax -> MysqlSyntax
joinOp joinType l r mOn = l <> " " <> joinType <> " " <> r <> fold mOn

tableOp :: MysqlSyntax -> MysqlSyntax -> MysqlSyntax -> MysqlSyntax
tableOp op l r = l <> " " <> op <> " " <> r

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

intoTableName :: MysqlTableNameSyntax -> MysqlSyntax
intoTableName (MysqlTableNameSyntax db name) =
  MysqlSyntax (foldMap (\t -> fromText t <> ".") db <> fromText name)

-- Insert values syntax to support runInsertRowReturning
data MysqlInsertValuesSyntax =
  FromExprs [[MysqlSyntax]] |
  FromSQL MysqlSyntax
  deriving stock (Eq)

-- Insert syntax to support runInsertRowReturning
data MysqlInsertSyntax = Insert MysqlTableNameSyntax [Text] MysqlInsertValuesSyntax
  deriving stock (Eq)

-- How we convert everything types defined in FromField to MySQL syntax

instance HasSqlValueSyntax MysqlSyntax Bool where
  sqlValueSyntax = \case
    True -> "TRUE"
    False -> "FALSE"

instance HasSqlValueSyntax MysqlSyntax Int8 where
  sqlValueSyntax = fromString . show

instance HasSqlValueSyntax MysqlSyntax Int16 where
  sqlValueSyntax = fromString . show

instance HasSqlValueSyntax MysqlSyntax Int32 where
  sqlValueSyntax = fromString . show

instance HasSqlValueSyntax MysqlSyntax Int64 where
  sqlValueSyntax = fromString . show

instance HasSqlValueSyntax MysqlSyntax Int where
  sqlValueSyntax = fromString . show

instance HasSqlValueSyntax MysqlSyntax Word8 where
  sqlValueSyntax = fromString . show

instance HasSqlValueSyntax MysqlSyntax Word16 where
  sqlValueSyntax = fromString . show

instance HasSqlValueSyntax MysqlSyntax Word32 where
  sqlValueSyntax = fromString . show

instance HasSqlValueSyntax MysqlSyntax Word64 where
  sqlValueSyntax = fromString . show

instance HasSqlValueSyntax MysqlSyntax Word where
  sqlValueSyntax = fromString . show

instance HasSqlValueSyntax MysqlSyntax Float where
  sqlValueSyntax = fromString . show

instance HasSqlValueSyntax MysqlSyntax Double where
  sqlValueSyntax = fromString . show

instance HasSqlValueSyntax MysqlSyntax Scientific where
  sqlValueSyntax = MysqlSyntax . scientificBuilder

-- Rational is exempted, because there's no good way to do this really

instance (HasSqlValueSyntax MysqlSyntax a) => HasSqlValueSyntax MysqlSyntax (Maybe a) where
  sqlValueSyntax = maybe (sqlValueSyntax SqlNull) sqlValueSyntax

instance HasSqlValueSyntax MysqlSyntax SqlNull where
  sqlValueSyntax = const "NULL"

instance HasSqlValueSyntax MysqlSyntax ByteString where
  sqlValueSyntax = quoteWrap . fromText . decodeUtf8

instance HasSqlValueSyntax MysqlSyntax Text where
  sqlValueSyntax = quoteWrap . fromText

instance HasSqlValueSyntax MysqlSyntax Day where
  sqlValueSyntax = quoteWrap . fromString . formatShow iso8601Format

instance HasSqlValueSyntax MysqlSyntax TimeOfDay where
  sqlValueSyntax = quoteWrap . fromString . formatShow iso8601Format

instance HasSqlValueSyntax MysqlSyntax LocalTime where
  sqlValueSyntax lt = quoteWrap (day <> " " <> tod)
    where
      day = fromString . formatShow iso8601Format . localDay $ lt
      tod = fromString . formatShow iso8601Format . localTimeOfDay $ lt

instance HasSqlValueSyntax MysqlSyntax NominalDiffTime where
  sqlValueSyntax d =
    let dWhole :: Int = abs . floor $ d
        hours :: Int = dWhole `div` 3600
        d' = dWhole - (hours * 3600)
        minutes = d' `div` 60
        seconds = abs d - fromIntegral ((hours * 3600) + (minutes * 60))
        secondsFixed :: Fixed E12 = fromRational . toRational $ seconds
      in
        MysqlSyntax ((if d < 0 then "-" else mempty) <>
                     (if hours < 10 then "0" else mempty) <>
                     (fromString . show $ hours) <>
                     ":" <>
                     (if minutes < 10 then "0" else mempty) <>
                     (fromString . show $ minutes) <>
                     ":" <>
                     (if secondsFixed < 10 then "0" else mempty) <>
                     (fromString . showFixed False $ secondsFixed))

instance HasSqlValueSyntax MysqlSyntax Value where
  sqlValueSyntax = quoteWrap . encodeToTextBuilder

-- Extra convenience instances

instance HasSqlValueSyntax MysqlSyntax Integer where
  sqlValueSyntax = fromString . show

instance HasSqlValueSyntax MysqlSyntax String where
  sqlValueSyntax = fromString

instance HasSqlValueSyntax MysqlSyntax TL.Text where
  sqlValueSyntax = MysqlSyntax . fromLazyText

-- Syntax defs

instance IsSql92FieldNameSyntax MysqlSyntax where
  unqualifiedField = backtickWrap . fromText
  qualifiedField qual f =
    (backtickWrap . fromText $ qual) <>
    "." <>
    unqualifiedField f

instance IsSql92QuantifierSyntax MysqlSyntax where
  quantifyOverAll = "ALL"
  quantifyOverAny = "ANY"

instance IsSql92DataTypeSyntax MysqlDataTypeSyntax where
  domainType t = MysqlDataTypeSyntax go go
    where
      go = backtickWrap . fromText $ t
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
  numericType mprec = MysqlDataTypeSyntax ("NUMERIC" <> prec) ("DECIMAL" <> prec)
    where
      prec = numPrec mprec
  decimalType mprec = MysqlDataTypeSyntax go go
    where
      go = "DECIMAL" <> numPrec mprec
  intType = MysqlDataTypeSyntax "INT" "INTEGER"
  smallIntType = MysqlDataTypeSyntax "SMALL INT" "INTEGER"
  floatType mprec = MysqlDataTypeSyntax def "DECIMAL"
    where
      def = "FLOAT" <> foldMap (bracketWrap . fromString . show) mprec
  doubleType = MysqlDataTypeSyntax "DOUBLE" "DECIMAL"
  realType = MysqlDataTypeSyntax "REAL" "DECIMAL"
  dateType = MysqlDataTypeSyntax "DATE" "DATE"
  timeType _ _ = MysqlDataTypeSyntax "TIME" "TIME"
  timestampType _ _ = MysqlDataTypeSyntax "TIMESTAMP" "DATETIME"

instance IsSql92ExtractFieldSyntax MysqlSyntax where
  secondsField = "SECOND"
  minutesField = "MINUTE"
  hourField = "HOUR"
  dayField = "DAY"
  monthField = "MONTH"
  yearField = "YEAR"

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
  likeE = binOp "LIKE"
  overlapsE = binOp "OVERLAPS"
  eqE = compOp "="
  neqE = compOp "<>"
  ltE = compOp "<"
  gtE = compOp ">"
  leE = compOp "<="
  geE = compOp ">="
  negateE = unaryOp "-"
  notE = unaryOp "NOT"
  existsE (MysqlSyntax e) = "EXISTS" <> bracketWrap e
  uniqueE (MysqlSyntax e) = "UNIQUE" <> bracketWrap e
  isNotNullE = postfix "IS NOT NULL"
  isNullE = postfix "IS NULL"
  isTrueE = postfix "IS TRUE"
  isFalseE = postfix "IS FALSE"
  isNotTrueE = postfix "IS NOT TRUE"
  isNotFalseE = postfix "IS NOT FALSE"
  isUnknownE = postfix "IS UNKNOWN"
  isNotUnknownE = postfix "IS NOT UNKNOWN"
  betweenE (MysqlSyntax arg) (MysqlSyntax lo) (MysqlSyntax hi) =
    bracketWrap arg <> " BETWEEN " <> bracketWrap lo <> " AND " <> bracketWrap hi
  valueE = id
  rowE vals = bracketWrap go
    where
      MysqlSyntax go = fold . intersperse ", " $ vals
  fieldE = id
  subqueryE (MysqlSyntax e) = bracketWrap e
  positionE (MysqlSyntax needle) (MysqlSyntax haystack) =
    "POSITION" <> bracketWrap (needle' <> " IN " <> haystack')
    where
      MysqlSyntax needle' = bracketWrap needle
      MysqlSyntax haystack' = bracketWrap haystack
  nullIfE (MysqlSyntax l) (MysqlSyntax r) = "NULLIF" <> bracketWrap (l <> ", " <> r)
  absE (MysqlSyntax x) = "ABS" <> bracketWrap x
  bitLengthE (MysqlSyntax x) = "BIT_LENGTH" <> bracketWrap x
  charLengthE (MysqlSyntax x) = "CHAR_LENGTH" <> bracketWrap x
  octetLengthE (MysqlSyntax x) = "OCTET_LENGTH" <> bracketWrap x
  coalesceE es = "COALESCE" <> bracketWrap go
    where
      MysqlSyntax go = fold . intersperse ", " $ es
  extractE (MysqlSyntax field) (MysqlSyntax from) =
    "EXTRACT" <> bracketWrap (field <> " FROM " <> go)
    where MysqlSyntax go = bracketWrap from
  castE (MysqlSyntax e) to =
    "CAST" <> bracketWrap (go <> " AS " <> castTarget)
    where
      MysqlSyntax go = bracketWrap e
      MysqlSyntax castTarget = mysqlTypeCast to
  caseE cases def =
    "CASE " <> foldMap go cases <> "ELSE " <> def <> " END"
    where go (cond, res) = "WHEN " <> cond <> " THEN " <> res <> " "
  currentTimestampE = "CURRENT_TIMESTAMP"
  defaultE = "DEFAULT"
  inE (MysqlSyntax e) es =
    bracketWrap e <> " IN " <> bracketWrap go
    where (MysqlSyntax go) = fold . intersperse ", " $ es
  trimE (MysqlSyntax x) = "TRIM" <> bracketWrap x
  lowerE (MysqlSyntax x) = "LOWER" <> bracketWrap x
  upperE (MysqlSyntax x) = "UPPER" <> bracketWrap x

instance IsSql92AggregationSetQuantifierSyntax MysqlSyntax where
  setQuantifierDistinct = "DISTINCT"
  setQuantifierAll = "ALL"

instance IsSql92AggregationExpressionSyntax MysqlSyntax where
  type Sql92AggregationSetQuantifierSyntax MysqlSyntax = MysqlSyntax
  countAllE = "COUNT(*)"
  countE = unaryAggregation "COUNT"
  avgE = unaryAggregation "AVG"
  sumE = unaryAggregation "SUM"
  minE = unaryAggregation "MIN"
  maxE = unaryAggregation "MAX"

instance IsSql92ProjectionSyntax MysqlSyntax where
  type Sql92ProjectionExpressionSyntax MysqlSyntax = MysqlSyntax
  projExprs exprs = fold . intersperse ", " . fmap go $ exprs
    where
      go (expr, name) =
        expr <> foldMap (\name' -> " AS " <> (backtickWrap . fromText $ name')) name

instance IsSql92TableNameSyntax MysqlTableNameSyntax where
  tableName = MysqlTableNameSyntax

instance IsSql92TableSourceSyntax MysqlSyntax where
  type Sql92TableSourceTableNameSyntax MysqlSyntax = MysqlTableNameSyntax
  type Sql92TableSourceSelectSyntax MysqlSyntax = MysqlSyntax
  type Sql92TableSourceExpressionSyntax MysqlSyntax = MysqlSyntax
  tableNamed = intoTableName
  tableFromSubSelect (MysqlSyntax s) = bracketWrap s
  tableFromValues =
    bracketWrap . fold . intersperse " UNION " . fmap (\vals -> "SELECT " <> go vals)
    where go = fold . intersperse ", " . fmap (\(MysqlSyntax e) -> "(" <> e <> ")")

instance IsSql92FromSyntax MysqlSyntax where
  type Sql92FromTableSourceSyntax MysqlSyntax = MysqlSyntax
  type Sql92FromExpressionSyntax MysqlSyntax = MysqlSyntax
  fromTable tableE = \case
    Nothing -> tableE
    Just (name, cols) ->
      tableE <>
      " AS " <>
      (backtickWrap . fromText $ name) <>
      foldMap (go . fold . intersperse ", " . fmap (backtickWrap . fromText)) cols
    where go (MysqlSyntax e) = bracketWrap e
  innerJoin = joinOp "JOIN"
  leftJoin = joinOp "LEFT JOIN"
  rightJoin = joinOp "RIGHT JOIN"

instance IsSql92GroupingSyntax MysqlSyntax where
  type Sql92GroupingExpressionSyntax MysqlSyntax = MysqlSyntax
  groupByExpressions = fold . intersperse ", "

instance IsSql92SelectTableSyntax MysqlSyntax where
  type Sql92SelectTableSelectSyntax MysqlSyntax = MysqlSyntax
  type Sql92SelectTableExpressionSyntax MysqlSyntax = MysqlSyntax
  type Sql92SelectTableProjectionSyntax MysqlSyntax = MysqlSyntax
  type Sql92SelectTableFromSyntax MysqlSyntax = MysqlSyntax
  type Sql92SelectTableGroupingSyntax MysqlSyntax = MysqlSyntax
  type Sql92SelectTableSetQuantifierSyntax MysqlSyntax = MysqlSyntax
  selectTableStmt quant proj from wher grouping having =
    "SELECT " <>
    foldMap (<> " ") quant <>
    proj <>
    foldMap (" FROM " <>) from <>
    foldMap (" WHERE " <>) wher <>
    foldMap (" GROUP BY " <>) grouping <>
    foldMap (" HAVING " <>) having
  unionTables isAll = if isAll
    then tableOp "UNION ALL"
    else tableOp "UNION"
  intersectTables _ = tableOp "INTERSECT"
  exceptTable _ = tableOp "EXCEPT"

instance IsSql92OrderingSyntax MysqlSyntax where
  type Sql92OrderingExpressionSyntax MysqlSyntax = MysqlSyntax
  ascOrdering = (<> " ASC")
  descOrdering = (<> " DESC")

instance IsSql92SelectSyntax MysqlSyntax where
  type Sql92SelectSelectTableSyntax MysqlSyntax = MysqlSyntax
  type Sql92SelectOrderingSyntax MysqlSyntax = MysqlSyntax
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


instance IsSql92InsertValuesSyntax MysqlInsertValuesSyntax where
  type Sql92InsertValuesExpressionSyntax MysqlInsertValuesSyntax = MysqlSyntax
  type Sql92InsertValuesSelectSyntax MysqlInsertValuesSyntax = MysqlSyntax
  insertSqlExpressions = FromExprs
  insertFromSql = FromSQL

instance IsSql92InsertSyntax MysqlInsertSyntax where
  type Sql92InsertValuesSyntax MysqlInsertSyntax = MysqlInsertValuesSyntax
  type Sql92InsertTableNameSyntax MysqlInsertSyntax = MysqlTableNameSyntax
  insertStmt = Insert

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

instance IsSql92DeleteSyntax MysqlSyntax where
  type Sql92DeleteTableNameSyntax MysqlSyntax = MysqlTableNameSyntax
  type Sql92DeleteExpressionSyntax MysqlSyntax = MysqlSyntax
  deleteStmt tblName _ wher =
    "DELETE FROM " <>
    intoTableName tblName <>
    foldMap (" WHERE " <>) wher
  deleteSupportsAlias = const False

instance IsSql92Syntax MysqlSyntax where
  type Sql92SelectSyntax MysqlSyntax = MysqlSyntax
  type Sql92InsertSyntax MysqlSyntax = MysqlInsertSyntax
  type Sql92UpdateSyntax MysqlSyntax = MysqlSyntax
  type Sql92DeleteSyntax MysqlSyntax = MysqlSyntax
  selectCmd = id
  insertCmd (Insert tblName fields values) =
    "INSERT INTO " <>
    intoTableName tblName <>
    "(" <>
    (fold . intersperse ", " . fmap (backtickWrap . fromText) $ fields) <>
    ")" <>
    go values
    where
      go = \case
        FromExprs exprs -> "VALUES " <> (fold . intersperse ", " . fmap go2 $ exprs)
        FromSQL sql -> sql
      go2 exprs = "(" <> (fold . intersperse ", " $ exprs) <> ")"
  updateCmd = id
  deleteCmd = id

instance IsSql99ConcatExpressionSyntax MysqlSyntax where
  concatE = \case
    [] -> mempty
    xs -> "CONCAT(" <> (fold . intersperse ", " $ xs) <> ")"
