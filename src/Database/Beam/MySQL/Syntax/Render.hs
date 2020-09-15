{-# LANGUAGE KindSignatures #-}

module Database.Beam.MySQL.Syntax.Render where

import           Data.Binary (Binary (put))
import qualified Data.Binary.Builder as Bin
import           Data.Bool (bool)
import           Data.ByteString.Builder.Scientific (FPFormat (Fixed),
                                                     formatScientificBuilder)
import           Data.Foldable (fold)
import           Data.HashSet (HashSet)
import           Data.String (IsString)
import           Data.Text (Text, pack)
import           Data.Text.Encoding (decodeLatin1, encodeUtf8)
import           Data.Time.Format (defaultTimeLocale, formatTime)
import           Data.Vector (Vector, toList)
import qualified Data.Vector as V
import           Database.Beam.MySQL.Syntax.DataType (MySQLDataTypeSyntax (..),
                                                      MySQLPrecision (..))
import           Database.Beam.MySQL.Syntax.Delete (MySQLDelete (DeleteStmt))
import           Database.Beam.MySQL.Syntax.Insert (MySQLInsert (..),
                                                    MySQLInsertValuesSyntax (..))
import           Database.Beam.MySQL.Syntax.Misc (MySQLAggregationSetQuantifierSyntax (..),
                                                  MySQLExtractFieldSyntax (..),
                                                  MySQLFieldNameSyntax (..),
                                                  MySQLQuantifierSyntax (..))
import           Database.Beam.MySQL.Syntax.Select (AggOp (..), BinOp (..),
                                                    CaseBranch (CaseBranch),
                                                    CompOp (..), ExpressionAnn,
                                                    MySQLExpressionSyntax (..),
                                                    MySQLFromSyntax (..),
                                                    MySQLGroupingSyntax (..),
                                                    MySQLOrderingSyntax (..),
                                                    MySQLProjectionSyntax (..),
                                                    MySQLSelect (..),
                                                    MySQLSelectTableSyntax (..),
                                                    MySQLTableNameSyntax (TableName),
                                                    MySQLTableSourceSyntax (..),
                                                    PostOp (..), PrefOp (..),
                                                    Projection (..),
                                                    TableHeader (..),
                                                    TableRowExpression (..))
import           Database.Beam.MySQL.Syntax.Update (FieldUpdate (..),
                                                    MySQLUpdate (..))
import           Database.Beam.MySQL.Syntax.Value (MySQLValueSyntax (..))
import           Database.MySQL.Base (Query (Query), QueryParam (render),
                                      renderParams)
import           Database.MySQL.Protocol.Escape (escapeBytes, escapeText)
import           Mason.Builder (BuilderFor, LazyByteStringBackend, byteString,
                                int16Dec, int32Dec, int64Dec, int8Dec,
                                integerDec, intersperse, lazyByteString,
                                string8, textUtf8, toLazyByteString, word16Dec,
                                word32Dec, word64Dec, word8Dec, wordDec)

newtype RenderErrorType = UnsupportedOperation Text
  deriving stock (Show)
  deriving newtype (Eq)

data RenderError = RenderError {
  errorType   :: !RenderErrorType,
  astFragment :: {-# UNPACK #-} !Text
  }
  deriving stock (Eq, Show)

renderSelect :: MySQLSelect -> Either RenderError (HashSet Text, Query)
renderSelect sel = do
  params <- traverse valueToParam . toList $ sel.ann.parameters
  let tabs = sel.ann.tablesInvolved
  sel' <- fmap (<> ";") . renderSelectInternal $ sel
  let query = renderParams (Query . toLazyByteString $ sel') params
  pure (tabs, query)

renderInsert :: MySQLInsert -> Either RenderError (HashSet Text, Query)
renderInsert (InsertStmt ann' tableName' cols vals) = do
  (params, tabs, tableName'') <- disassembleMeta ann' tableName'
  vals' <- renderInsertValues vals
  let insert' =
        "INSERT INTO " <>
        tableName'' <>
        (bracketWrap . intersperse ", " . toList . fmap textUtf8 $ cols) <>
        vals' <>
        ";"
  let query = renderParams (Query . toLazyByteString $ insert') params
  pure (tabs, query)

renderUpdate :: MySQLUpdate -> Either RenderError (HashSet Text, Query)
renderUpdate (UpdateStmt ann' tableName' upds wher') = do
  (params, tabs, tableName'') <- disassembleMeta ann' tableName'
  upds' <- traverse renderFieldUpdate upds
  wher'' <- traverse renderExpr wher'
  let update' =
        "UPDATE " <>
        tableName'' <>
        go upds' <>
        foldMap (" WHERE " <>) wher'' <>
        ";"
  let query = renderParams (Query . toLazyByteString $ update') params
  pure (tabs, query)
  where
    go ::
      Vector (BuilderFor LazyByteStringBackend) ->
      BuilderFor LazyByteStringBackend
    go v = if V.length v == 0
      then mempty
      else " SET " <> (intersperse ", " . toList $ v)

renderDelete :: MySQLDelete -> Either RenderError (HashSet Text, Query)
renderDelete (DeleteStmt ann' tableName' wher') = do
  (params, tabs, tableName'') <- disassembleMeta ann' tableName'
  wher'' <- traverse renderExpr wher'
  let delete' =
        "DELETE FROM " <>
        tableName'' <>
        foldMap (" WHERE " <>) wher'' <>
        ";"
  let query = renderParams (Query . toLazyByteString $ delete') params
  pure (tabs, query)

-- Helpers

renderFieldUpdate ::
  FieldUpdate ->
  Either RenderError (BuilderFor LazyByteStringBackend)
renderFieldUpdate (FieldUpdate nam e) = do
  nam' <- renderFieldName nam
  e' <- renderExpr e
  pure $ nam' <> "=" <> e'

renderInsertValues ::
  MySQLInsertValuesSyntax ->
  Either RenderError (BuilderFor LazyByteStringBackend)
renderInsertValues = \case
  InsertSQLExpressions _ rti -> do
    rti' <- traverse renderTableRowExpr rti
    pure $ " VALUES " <> (intersperse ", " . toList $ rti')
  InsertFromSQL _ sel -> renderSelectInternal sel

disassembleMeta ::
  ExpressionAnn ->
  MySQLTableNameSyntax ->
  Either RenderError ([P], HashSet Text, BuilderFor LazyByteStringBackend)
disassembleMeta ann' tableName' = do
  params <- traverse valueToParam . toList $ ann'.parameters
  let tabs = ann'.tablesInvolved
  tableName'' <- renderTableName tableName'
  pure (params, tabs, tableName'')

newtype P = P (BuilderFor LazyByteStringBackend)

instance QueryParam P where
  {-# INLINABLE render #-}
  render (P b) = put . toLazyByteString $ b

valueToParam :: MySQLValueSyntax -> Either RenderError P
valueToParam = fmap P . \case
  VBool b -> pure . bool "FALSE" "TRUE" $ b
  VInt8 i -> pure . int8Dec $ i
  VInt16 i -> pure . int16Dec $ i
  VInt32 i -> pure . int32Dec $ i
  VInt64 i -> pure . int64Dec $ i
  VWord8 w -> pure . word8Dec $ w
  VWord16 w -> pure . word16Dec $ w
  VWord32 w -> pure . word32Dec $ w
  VWord64 w -> pure . word64Dec $ w
  VScientific s ->
    pure .
      lazyByteString .
      Bin.toLazyByteString .
      formatScientificBuilder Fixed Nothing $ s
  VNothing -> pure "NULL"
  VNull -> pure "NULL"
  VByteString b -> pure . quoteWrap . byteString . escapeBytes $ b
  VText t -> do
    -- This ensures that all text we receive can be represented.
    -- Gory, but unavoidable.
    let t' = decodeLatin1 . encodeUtf8 . escapeText $ t
    pure . quoteWrap . textUtf8 $ t'
  VDay d ->
    pure . quoteWrap . string8 . formatTime defaultTimeLocale "%F" $ d
  VLocalTime lt ->
    pure. quoteWrap . string8 . formatTime defaultTimeLocale "%F %T%Q" $ lt
  VTimeOfDay tod ->
    pure . quoteWrap . string8 . formatTime defaultTimeLocale "%T%Q" $ tod

renderTableName ::
  MySQLTableNameSyntax -> Either RenderError (BuilderFor LazyByteStringBackend)
renderTableName (TableName mSchema nam) = do
  let mSchema' = fmap textUtf8 mSchema
  let nam' = textUtf8 nam
  pure $ fold mSchema' <> backtickWrap nam'

renderExpr ::
  MySQLExpressionSyntax -> Either RenderError (BuilderFor LazyByteStringBackend)
renderExpr = \case
  Placeholder{} -> pure "?"
  Row _ exprs' -> do
    exprs'' <- traverse renderExpr exprs'
    pure . bracketWrap . intersperse ", " . toList $ exprs''
  Coalesce _ exprs' -> do
    exprs'' <- traverse renderExpr exprs'
    pure . ("COALESCE " <>) . bracketWrap . intersperse ", " . toList $ exprs''
  Case _ cases' def' -> do
    cases'' <- traverse renderCaseBranch cases'
    def'' <- renderExpr def'
    pure $
      "CASE " <>
      (intersperse " " . toList $ cases'') <>
      "ELSE " <>
      def'' <>
      " END"
  Field _ fieldName' -> renderFieldName fieldName'
  BinaryOperation _ op' l' r' -> do
    op'' <- renderBinOp op'
    l'' <- renderExpr l'
    r'' <- renderExpr r'
    pure $
      bracketWrap l'' <>
      " " <>
      op'' <>
      " " <>
      bracketWrap r''
  ComparisonOperation _ op q l r -> do
    op' <- renderCompOp op
    q' <- traverse renderQuantifier q
    l' <- renderExpr l
    r' <- renderExpr r
    pure $
      bracketWrap l' <>
      " " <>
      op' <>
      fold q' <>
      " " <>
      bracketWrap r'
  PrefixOperation _ op e -> do
    op' <- renderPrefOp op
    e' <- renderExpr e
    pure $ op' <> bracketWrap e'
  PostfixOperation _ op e -> do
    op' <- renderPostOp op
    e' <- renderExpr e
    pure $ bracketWrap e' <> op'
  NullIf _ e ifNull' -> do
    e' <- renderExpr e
    ifNull'' <- renderExpr ifNull'
    pure $
      "NULLIF" <>
      bracketWrap (e' <> ", " <> ifNull'')
  Position _ needle' haystack' -> do
    needle'' <- renderExpr needle'
    haystack'' <- renderExpr haystack'
    pure $
      "POSITION" <>
      bracketWrap (bracketWrap needle'' <> " IN " <> bracketWrap haystack'')
  Cast _ target' e -> do
    target'' <- renderCastTarget target'
    e' <- renderExpr e
    pure $
      "CAST" <>
      bracketWrap (bracketWrap e' <> " AS " <> target'')
  Extract _ field' e -> do
    field'' <- renderExtractField field'
    e' <- renderExpr e
    pure $
      "EXTRACT" <>
      bracketWrap (field'' <> " FROM " <> e')
  CurrentTimestamp _ -> pure "CURRENT_TIMESTAMP"
  Default _ -> pure "DEFAULT"
  In _ e es -> do
    e' <- renderExpr e
    es' <- traverse renderExpr es
    pure $
      bracketWrap e' <>
      " IN " <>
      (bracketWrap . intersperse ", " . toList $ es')
  Between _ e lo' hi' -> do
    e' <- renderExpr e
    lo'' <- renderExpr lo'
    hi'' <- renderExpr hi'
    pure $
      bracketWrap e' <>
      " BETWEEN " <>
      bracketWrap lo'' <>
      " AND " <>
      bracketWrap hi''
  Exists _ sel -> do
    sel' <- renderSelectInternal sel
    pure $
      "EXISTS" <>
      bracketWrap sel'
  Unique _ sel -> do
    sel' <- renderSelectInternal sel
    pure $
      "UNIQUE" <>
      bracketWrap sel'
  Subquery _ sel -> do
    sel' <- renderSelectInternal sel
    pure . bracketWrap $ sel'
  CountAll _ -> pure "COUNT(*)"
  Aggregation _ op quant e -> do
    op' <- renderAggOp op
    quant' <- traverse renderAggregationSetQuantifier quant
    e' <- renderExpr e
    pure $
      op' <>
      bracketWrap (foldMap (<> " ") quant' <> e')
  Concat _ es -> do
    es' <- traverse renderExpr es
    if V.length es == 0
    then pure mempty
    else pure $
      "CONCAT" <>
      bracketWrap (intersperse ", " . toList $ es')
  LastInsertId -> pure "last_insert_id()"

renderAggregationSetQuantifier ::
  MySQLAggregationSetQuantifierSyntax ->
  Either RenderError (BuilderFor LazyByteStringBackend)
renderAggregationSetQuantifier = pure . \case
  SetDistinct -> "DISTINCT"
  SetAll -> "ALL"

renderAggOp :: AggOp -> Either RenderError (BuilderFor LazyByteStringBackend)
renderAggOp = pure . \case
  Count -> "COUNT"
  Avg -> "AVG"
  Sum -> "SUM"
  Min -> "MIN"
  Max -> "MAX"

renderSelectInternal ::
  MySQLSelect ->
  Either RenderError (BuilderFor LazyByteStringBackend)
renderSelectInternal sel = do
  table' <- renderSelectTable sel.table
  orderings' <- traverse renderOrdering sel.orderings
  let limit' = fmap integerDec sel.limit
  let offset' = fmap integerDec sel.offset
  pure $
    table' <>
    go orderings' <>
    go2 limit' offset'
    where
      go ::
        Vector (BuilderFor LazyByteStringBackend) ->
        BuilderFor LazyByteStringBackend
      go v = if V.length v == 0
        then mempty
        else " ORDER BY " <> (intersperse ", " . toList $ v)
      go2 ::
        Maybe (BuilderFor LazyByteStringBackend) ->
        Maybe (BuilderFor LazyByteStringBackend) ->
        BuilderFor LazyByteStringBackend
      go2 limit' offset' = case limit' of
        Nothing -> mempty
        Just lim -> case offset' of
          Nothing  -> " LIMIT " <> lim
          Just off -> " LIMIT " <> off <> ", " <> lim

renderOrdering ::
  MySQLOrderingSyntax ->
  Either RenderError (BuilderFor LazyByteStringBackend)
renderOrdering = \case
  AscOrdering _ e -> do
    e' <- renderExpr e
    pure (e' <> " ASC")
  DescOrdering _ e -> do
    e' <- renderExpr e
    pure (e' <> " DESC")

renderSelectTable ::
  MySQLSelectTableSyntax ->
  Either RenderError (BuilderFor LazyByteStringBackend)
renderSelectTable = \case
  UnionTablesAll _ l r -> do
    l' <- renderSelectTable l
    r' <- renderSelectTable r
    pure $ l' <> " UNION ALL " <> r'
  UnionTablesDistinct _ l r -> do
    l' <- renderSelectTable l
    r' <- renderSelectTable r
    pure $ l' <> "UNION DISTINCT " <> r'
  st@IntersectTables{} ->
    Left . RenderError (UnsupportedOperation "INTERSECT") . pack . show $ st
  st@ExceptTable{} ->
    Left . RenderError (UnsupportedOperation "EXCEPT") . pack . show $ st
  st -> do
    q <- traverse renderAggregationSetQuantifier st.quantifier
    proj <- renderProjection st.projection
    from' <- traverse renderFrom st.from
    wher' <- traverse renderExpr st.wher
    grouping' <- traverse renderGrouping st.grouping
    having' <- traverse renderExpr st.having
    pure $
      "SELECT " <>
      foldMap (" " <>) q <>
      proj <>
      foldMap (" FROM " <>) from' <>
      foldMap (" WHERE " <>) wher' <>
      foldMap (" GROUP BY " <>) grouping' <>
      foldMap (" HAVING " <>) having'

renderGrouping ::
  MySQLGroupingSyntax ->
  Either RenderError (BuilderFor LazyByteStringBackend)
renderGrouping (GroupByExpressions _ es) =
  intersperse ", " . toList <$> traverse renderExpr es

renderFrom ::
  MySQLFromSyntax ->
  Either RenderError (BuilderFor LazyByteStringBackend)
renderFrom = \case
  FromTable src hdr -> do
    src' <- renderTableSource src
    hdr' <- renderTableHeader hdr
    pure $ src' <> hdr'
  InnerJoin _ l r c -> do
    (l', r', c') <- renderJoinParts l r c
    pure $ l' <> " JOIN " <> r' <> fold c'
  LeftJoin _ l r c -> do
    (l', r', c') <- renderJoinParts l r c
    pure $ l' <> " LEFT JOIN " <> r' <> fold c'
  RightJoin _ l r c -> do
    (l', r', c') <- renderJoinParts l r c
    pure $ l' <> " RIGHT JOIN " <> r' <> fold c'

renderJoinParts :: (Traversable t) =>
  MySQLFromSyntax ->
  MySQLFromSyntax ->
  t MySQLExpressionSyntax ->
  Either RenderError (BuilderFor LazyByteStringBackend,
                      BuilderFor LazyByteStringBackend,
                      t (BuilderFor LazyByteStringBackend))
renderJoinParts l r c = do
  l' <- renderFrom l
  r' <- renderFrom r
  c' <- traverse renderExpr c
  pure (l', r', c')

renderTableHeader ::
  TableHeader ->
  Either RenderError (BuilderFor LazyByteStringBackend)
renderTableHeader = pure . \case
  Anonymous -> mempty
  TableNameOnly nam -> " AS " <> (backtickWrap . textUtf8 $ nam)
  TableAndColumns nam cols ->
    " AS " <>
    (backtickWrap . textUtf8 $ nam) <>
    (bracketWrap . intersperse ", " . toList . fmap (backtickWrap . textUtf8) $ cols)

renderTableSource ::
  MySQLTableSourceSyntax ->
  Either RenderError (BuilderFor LazyByteStringBackend)
renderTableSource = \case
  TableNamed nam -> renderTableName nam
  TableFromSubSelect sel -> bracketWrap <$> renderSelectInternal sel
  TableFromValues _ rs ->
    bracketWrap . intersperse " UNION " . toList . fmap ("SELECT " <>) <$>
      traverse renderTableRowExpr rs

renderTableRowExpr ::
  TableRowExpression ->
  Either RenderError (BuilderFor LazyByteStringBackend)
renderTableRowExpr (TableRowExpression v) = do
  v' <- traverse renderExpr v
  pure . intersperse ", " . fmap bracketWrap . toList $ v'

renderProjection ::
  MySQLProjectionSyntax ->
  Either RenderError (BuilderFor LazyByteStringBackend)
renderProjection (ProjectExpressions _ projs) = do
  projs' <- traverse renderProjectionInner projs
  pure . intersperse ", " . toList $ projs'

renderProjectionInner ::
  Projection ->
  Either RenderError (BuilderFor LazyByteStringBackend)
renderProjectionInner (Projection e l) = do
  e' <- renderExpr e
  pure $ e' <> foldMap ((" AS " <>) . backtickWrap . textUtf8) l

renderExtractField ::
  MySQLExtractFieldSyntax ->
  Either RenderError (BuilderFor LazyByteStringBackend)
renderExtractField = pure . \case
  SecondsField -> "SECOND"
  MinutesField -> "MINUTE"
  HourField -> "HOUR"
  DayField -> "DAY"
  MonthField -> "MONTH"
  YearField -> "YEAR"

renderCastTarget ::
  MySQLDataTypeSyntax ->
  Either RenderError (BuilderFor LazyByteStringBackend)
renderCastTarget = pure . \case
  DomainType t -> backtickWrap . textUtf8 $ t
  CharType _ _ -> "CHAR"
  VarCharType _ _ -> "CHAR"
  NationalCharType _ -> "CHAR"
  NationalVarCharType _ -> "CHAR"
  BitType _ -> "BINARY"
  VarBitType _ -> "BINARY"
  NumericType mPrec -> "DECIMAL" <> renderNumPrec mPrec
  DecimalType mPrec -> "DECIMAL" <> renderNumPrec mPrec
  IntType -> "INTEGER"
  SmallIntType -> "INTEGER"
  FloatType prec -> "FLOAT" <> foldMap (bracketWrap . wordDec) prec
  DoubleType -> "DECIMAL"
  RealType -> "DECIMAL"
  DateType -> "DATE"
  TimeType -> "TIME"
  TimestampType -> "DATETIME"

renderNumPrec :: MySQLPrecision -> BuilderFor LazyByteStringBackend
renderNumPrec = \case
  None -> mempty
  DigitsOnly d -> bracketWrap . wordDec $ d
  DigitsScale d s -> bracketWrap (wordDec d <> ", " <> wordDec s)

renderPostOp :: PostOp -> Either RenderError (BuilderFor LazyByteStringBackend)
renderPostOp = pure . \case
  GIsNull -> "IS NULL"
  GIsNotNull -> "IS NOT NULL"
  GIsTrue -> "IS TRUE"
  GIsNotTrue -> "IS NOT TRUE"
  GIsFalse -> "IS FALSE"
  GIsNotFalse -> "IS NOT FALSE"
  GIsUnknown -> "IS UNKNOWN"
  GIsNotUnknown -> "IS NOT UNKNOWN"

renderPrefOp :: PrefOp -> Either RenderError (BuilderFor LazyByteStringBackend)
renderPrefOp = pure . \case
  LNot -> "NOT"
  NNegate -> "-"
  TCharLength -> "CHAR_LENGTH"
  TOctetLength -> "OCTET_LENGTH"
  BBitLength -> "BIT_LENGTH"
  TLower -> "LOWER"
  TUpper -> "UPPER"
  TTrim -> "TRIM"
  NAbs -> "ABS"

renderCompOp :: CompOp -> Either RenderError (BuilderFor LazyByteStringBackend)
renderCompOp = pure . \case
  CEq -> "="
  CNe -> "<>"
  CLe -> "<="
  CLt -> "<"
  CGe -> ">="
  CGt -> ">"

renderQuantifier ::
  MySQLQuantifierSyntax -> Either RenderError (BuilderFor LazyByteStringBackend)
renderQuantifier = pure . \case
  Any -> "ANY"
  All -> "ALL"

renderBinOp :: BinOp -> Either RenderError (BuilderFor LazyByteStringBackend)
renderBinOp = pure . \case
  LAnd -> "AND"
  LOr -> "OR"
  NAdd -> "+"
  NMul -> "*"
  NSub -> "-"
  NDiv -> "/"
  NMod -> "%"
  GLike -> "LIKE"
  GOverlaps -> "OVERLAPS"

renderFieldName ::
  MySQLFieldNameSyntax -> Either RenderError (BuilderFor LazyByteStringBackend)
renderFieldName = pure . \case
  UnqualifiedField f -> backtickWrap . textUtf8 $ f
  QualifiedField t f ->
    (backtickWrap . textUtf8 $ t) <> (backtickWrap . textUtf8 $ f)

renderCaseBranch :: CaseBranch -> Either RenderError (BuilderFor LazyByteStringBackend)
renderCaseBranch (CaseBranch c a) = do
  c' <- renderExpr c
  a' <- renderExpr a
  pure $
    "WHEN " <>
    c' <>
    " THEN " <>
    a'

wrap :: (Semigroup s) => s -> s -> s -> s
wrap l r v = l <> v <> r

bracketWrap :: (IsString s, Semigroup s) => s -> s
bracketWrap = wrap "(" ")"

quoteWrap :: (IsString s, Semigroup s) => s -> s
quoteWrap = wrap "'" "'"

backtickWrap :: (IsString s, Semigroup s) => s -> s
backtickWrap = wrap "`" "`"
