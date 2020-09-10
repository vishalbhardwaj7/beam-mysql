-- Due to RDP plugin.
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

module Database.Beam.MySQL.Syntax.Select where

import           Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import           Data.Text (Text)
import           Data.Vector (Vector, foldl', foldl1', fromList, map, singleton)
import           Database.Beam.Backend.SQL (IsSql92AggregationExpressionSyntax (..),
                                            IsSql92ExpressionSyntax (..),
                                            IsSql92FromSyntax (..),
                                            IsSql92GroupingSyntax (..),
                                            IsSql92OrderingSyntax (..),
                                            IsSql92ProjectionSyntax (..),
                                            IsSql92SelectSyntax (..),
                                            IsSql92SelectTableSyntax (..),
                                            IsSql92TableNameSyntax (..),
                                            IsSql92TableSourceSyntax (..))
import           Database.Beam.MySQL.Syntax.DataType (MySQLDataTypeSyntax)
import           Database.Beam.MySQL.Syntax.Misc (MySQLAggregationSetQuantifierSyntax,
                                                  MySQLExtractFieldSyntax,
                                                  MySQLFieldNameSyntax (QualifiedField, UnqualifiedField),
                                                  MySQLQuantifierSyntax)
import           Database.Beam.MySQL.Syntax.Value (MySQLValueSyntax)
import qualified Language.C.Inline as C
import           Prelude hiding (map)

C.include "<mysql/mysql.h>"

data MySQLOrderingSyntax =
  AscOrdering {
    ann  :: !ExpressionAnn,
    expr :: MySQLExpressionSyntax
    } |
  DescOrdering {
    ann  :: !ExpressionAnn,
    expr :: MySQLExpressionSyntax
    }
  deriving stock (Eq, Show)

instance IsSql92OrderingSyntax MySQLOrderingSyntax where
  type Sql92OrderingExpressionSyntax MySQLOrderingSyntax =
    MySQLExpressionSyntax
  {-# INLINABLE ascOrdering #-}
  ascOrdering e = AscOrdering (getAnn e) e
  {-# INLINABLE descOrdering #-}
  descOrdering e = DescOrdering (getAnn e) e

data MySQLSelectTableSyntax =
  SelectTableStatement {
    ann        :: !ExpressionAnn,
    quantifier :: !(Maybe MySQLAggregationSetQuantifierSyntax),
    projection :: {-# UNPACK #-} !MySQLProjectionSyntax,
    from       :: !(Maybe MySQLFromSyntax),
    wher       :: !(Maybe MySQLExpressionSyntax),
    grouping   :: !(Maybe MySQLGroupingSyntax),
    having     :: !(Maybe MySQLExpressionSyntax)
    } |
  UnionTables {
    ann    :: !ExpressionAnn,
    isAll  :: !Bool,
    lTable :: MySQLSelectTableSyntax,
    rTable :: MySQLSelectTableSyntax
    } |
  IntersectTables {
    ann    :: !ExpressionAnn,
    lTable :: MySQLSelectTableSyntax,
    rTable :: MySQLSelectTableSyntax
    } |
  ExceptTable {
    ann    :: !ExpressionAnn,
    lTable :: MySQLSelectTableSyntax,
    rTable :: MySQLSelectTableSyntax
    }
  deriving stock (Eq, Show)

instance IsSql92SelectTableSyntax MySQLSelectTableSyntax where
  type Sql92SelectTableSelectSyntax MySQLSelectTableSyntax =
    MySQLSelectSyntax
  type Sql92SelectTableExpressionSyntax MySQLSelectTableSyntax =
    MySQLExpressionSyntax
  type Sql92SelectTableProjectionSyntax MySQLSelectTableSyntax =
    MySQLProjectionSyntax
  type Sql92SelectTableFromSyntax MySQLSelectTableSyntax =
    MySQLFromSyntax
  type Sql92SelectTableGroupingSyntax MySQLSelectTableSyntax =
    MySQLGroupingSyntax
  type Sql92SelectTableSetQuantifierSyntax MySQLSelectTableSyntax =
    MySQLAggregationSetQuantifierSyntax
  {-# INLINABLE selectTableStmt #-}
  selectTableStmt quant proj from' wher' grouping' having' =
    SelectTableStatement ann' quant proj from' wher' grouping' having'
    where
      ann' :: ExpressionAnn
      ann' =
        proj.ann <>
        foldMap fromAnn from' <>
        foldMap getAnn wher' <>
        foldMap (.ann) grouping' <>
        foldMap getAnn having'
  {-# INLINABLE unionTables #-}
  unionTables isAll' lOp rOp =
    UnionTables (lOp.ann <> rOp.ann) isAll' lOp rOp
  {-# INLINABLE intersectTables #-}
  intersectTables _ lOp rOp =
    IntersectTables (lOp.ann <> rOp.ann) lOp rOp
  {-# INLINABLE exceptTable #-}
  exceptTable _ lOp rOp =
    ExceptTable (lOp.ann <> rOp.ann) lOp rOp

-- Used to mark an expression as either pure (meaning, rerunnable any number of
-- times) or impure (side-effecting).
data Purity = Pure | Impure
 deriving stock (Eq, Show)

instance Semigroup Purity where
  Pure <> x = x
  Impure <> _ = Impure

instance Monoid Purity where
  mempty = Pure

data ExpressionAnn = ExpressionAnn {
  purity         :: !Purity,
  parameters     :: {-# UNPACK #-} !(Vector MySQLValueSyntax),
  tablesInvolved :: !(HashSet Text)
  }
  deriving stock (Eq, Show)

instance Semigroup ExpressionAnn where
  ExpressionAnn p v t <> ExpressionAnn p' v' t' =
    ExpressionAnn (p <> p') (v <> v') (t <> t')

instance Monoid ExpressionAnn where
  mempty = ExpressionAnn mempty mempty mempty

foldAnn :: Vector ExpressionAnn -> ExpressionAnn
foldAnn = foldl1' (<>)

fromValues :: MySQLValueSyntax -> ExpressionAnn
fromValues p = ExpressionAnn Pure (singleton p) mempty

data BinOp =
  LAnd |
  LOr |
  NAdd |
  NMul |
  NSub |
  NDiv |
  NMod |
  GLike |
  GOverlaps
  deriving stock (Eq, Show)

data CompOp =
  CEq |
  CNe |
  CLe |
  CLt |
  CGe |
  CGt
  deriving stock (Eq, Show)

data PrefOp =
  LNot |
  NNegate |
  TCharLength |
  TOctetLength |
  BBitLength |
  TLower |
  TUpper |
  TTrim |
  NAbs
  deriving stock (Eq, Show)

data PostOp =
  GIsNull |
  GIsNotNull |
  GIsTrue |
  GIsNotTrue |
  GIsFalse |
  GIsNotFalse |
  GIsUnknown |
  GIsNotUnknown
  deriving stock (Eq, Show)

data AggOp =
  Count |
  Avg |
  Sum |
  Min |
  Max
  deriving stock (Eq, Show)

data MySQLExpressionSyntax =
  Placeholder {
    ann :: !ExpressionAnn
    } |
  Row {
    ann   :: !ExpressionAnn,
    exprs :: {-# UNPACK #-} !(Vector MySQLExpressionSyntax)
    } |
  Coalesce {
    ann   :: !ExpressionAnn,
    exprs :: {-# UNPACK #-} !(Vector MySQLExpressionSyntax)
    } |
  Case {
    ann :: !ExpressionAnn,
    cases ::
      {-# UNPACK #-} !(Vector (MySQLExpressionSyntax, MySQLExpressionSyntax)),
    defaultCase :: MySQLExpressionSyntax
    } |
  Field {
    ann       :: !ExpressionAnn,
    fieldName :: !MySQLFieldNameSyntax
    } |
  BinaryOperation {
    ann      :: !ExpressionAnn,
    binOp    :: !BinOp,
    lOperand :: MySQLExpressionSyntax,
    rOperand :: MySQLExpressionSyntax
    } |
  ComparisonOperation {
    ann        :: !ExpressionAnn,
    compOp     :: !CompOp,
    quantifier :: !(Maybe MySQLQuantifierSyntax),
    lOperand   :: MySQLExpressionSyntax,
    rOperand   :: MySQLExpressionSyntax
    } |
  PrefixOperation {
    ann     :: !ExpressionAnn,
    prefOp  :: !PrefOp,
    operand :: MySQLExpressionSyntax
    } |
  PostfixOperation {
    ann     :: !ExpressionAnn,
    postOp  :: !PostOp,
    operand :: MySQLExpressionSyntax
    } |
  NullIf {
    ann    :: !ExpressionAnn,
    expr   :: MySQLExpressionSyntax,
    ifNull :: MySQLExpressionSyntax
    } |
  Position {
    ann      :: !ExpressionAnn,
    needle   :: MySQLExpressionSyntax,
    haystack :: MySQLExpressionSyntax
    } |
  Cast {
    ann    :: !ExpressionAnn,
    target :: !MySQLDataTypeSyntax,
    expr   :: MySQLExpressionSyntax
    } |
  Extract {
    ann   :: !ExpressionAnn,
    field :: !MySQLExtractFieldSyntax,
    expr  :: MySQLExpressionSyntax
    } |
  CurrentTimestamp {
    ann :: !ExpressionAnn
    } |
  Default {
    ann :: !ExpressionAnn
    } |
  In {
    ann   :: !ExpressionAnn,
    expr  :: MySQLExpressionSyntax,
    exprs :: {-# UNPACK #-} !(Vector MySQLExpressionSyntax)
    } |
  Between {
    ann  :: !ExpressionAnn,
    expr :: MySQLExpressionSyntax,
    lo   :: MySQLExpressionSyntax,
    hi   :: MySQLExpressionSyntax
    } |
  Exists {
    ann    :: !ExpressionAnn,
    select :: !MySQLSelectSyntax
    } |
  Unique {
    ann    :: !ExpressionAnn,
    select :: !MySQLSelectSyntax
    } |
  Subquery {
    ann    :: !ExpressionAnn,
    select :: !MySQLSelectSyntax
    } |
  CountAll {
    ann :: !ExpressionAnn
    } |
  Aggregation {
    ann           :: !ExpressionAnn,
    aggregationOp :: !AggOp,
    setQuantifier :: !(Maybe MySQLAggregationSetQuantifierSyntax),
    expr          :: MySQLExpressionSyntax
    }
  deriving stock (Eq, Show)

getAnn :: MySQLExpressionSyntax -> ExpressionAnn
getAnn = ann

instance IsSql92ExpressionSyntax MySQLExpressionSyntax where
  type Sql92ExpressionValueSyntax MySQLExpressionSyntax =
    MySQLValueSyntax
  type Sql92ExpressionFieldNameSyntax MySQLExpressionSyntax =
    MySQLFieldNameSyntax
  type Sql92ExpressionQuantifierSyntax MySQLExpressionSyntax =
    MySQLQuantifierSyntax
  type Sql92ExpressionCastTargetSyntax MySQLExpressionSyntax =
    MySQLDataTypeSyntax
  type Sql92ExpressionExtractFieldSyntax MySQLExpressionSyntax =
    MySQLExtractFieldSyntax
  type Sql92ExpressionSelectSyntax MySQLExpressionSyntax =
    MySQLSelectSyntax
  {-# INLINABLE valueE #-}
  valueE = Placeholder . fromValues
  {-# INLINABLE rowE #-}
  rowE = (\v -> Row (foldAnn . map getAnn $ v) v) . fromList
  {-# INLINABLE coalesceE #-}
  coalesceE = (\v -> Coalesce (foldAnn . map getAnn $ v) v) . fromList
  {-# INLINABLE caseE #-}
  caseE cases' def = Case ann' casesV def
    where
      casesV :: Vector (MySQLExpressionSyntax, MySQLExpressionSyntax)
      casesV = fromList cases'
      ann' :: ExpressionAnn
      ann' = foldl' go (getAnn def) casesV
      go ::
        ExpressionAnn ->
        (MySQLExpressionSyntax, MySQLExpressionSyntax) ->
        ExpressionAnn
      go ann'' (e1, e2) = ann'' <> getAnn e1 <> getAnn e2
  {-# INLINABLE fieldE #-}
  fieldE = \case
    f@(QualifiedField nam _) ->
      Field (ExpressionAnn Impure mempty (HS.singleton nam)) f
    f@(UnqualifiedField _) ->
      Field (ExpressionAnn Impure mempty mempty) f
  {-# INLINABLE andE #-}
  andE = makeBinOp LAnd
  {-# INLINABLE orE #-}
  orE = makeBinOp LOr
  {-# INLINABLE addE #-}
  addE = makeBinOp NAdd
  {-# INLINABLE mulE #-}
  mulE = makeBinOp NMul
  {-# INLINABLE subE #-}
  subE = makeBinOp NSub
  {-# INLINABLE divE #-}
  divE = makeBinOp NDiv
  {-# INLINABLE likeE #-}
  likeE = makeBinOp GLike
  {-# INLINABLE modE #-}
  modE = makeBinOp NMod
  {-# INLINABLE overlapsE #-}
  overlapsE = makeBinOp GOverlaps
  {-# INLINABLE nullIfE #-}
  nullIfE l r = NullIf (getAnn l <> getAnn r) l r
  {-# INLINABLE positionE #-}
  positionE needle' haystack' =
    Position (getAnn needle' <> getAnn haystack') needle' haystack'
  {-# INLINABLE eqE #-}
  eqE = makeCompOp CEq
  {-# INLINABLE neqE #-}
  neqE = makeCompOp CNe
  {-# INLINABLE ltE #-}
  ltE = makeCompOp CLt
  {-# INLINABLE gtE #-}
  gtE = makeCompOp CGt
  {-# INLINABLE leE #-}
  leE = makeCompOp CLe
  {-# INLINABLE geE #-}
  geE = makeCompOp CGe
  {-# INLINABLE castE #-}
  castE e dt = Cast (getAnn e) dt e
  {-# INLINABLE notE #-}
  notE = makePrefixOp LNot
  {-# INLINABLE negateE #-}
  negateE = makePrefixOp NNegate
  {-# INLINABLE isNullE #-}
  isNullE = makePostfixOp GIsNull
  {-# INLINABLE isNotNullE #-}
  isNotNullE = makePostfixOp GIsNotNull
  {-# INLINABLE isTrueE #-}
  isTrueE = makePostfixOp GIsTrue
  {-# INLINABLE isNotTrueE #-}
  isNotTrueE = makePostfixOp GIsNotTrue
  {-# INLINABLE isFalseE #-}
  isFalseE = makePostfixOp GIsFalse
  {-# INLINABLE isNotFalseE #-}
  isNotFalseE = makePostfixOp GIsNotFalse
  {-# INLINABLE isUnknownE #-}
  isUnknownE = makePostfixOp GIsUnknown
  {-# INLINABLE isNotUnknownE #-}
  isNotUnknownE = makePostfixOp GIsNotUnknown
  {-# INLINABLE charLengthE #-}
  charLengthE = makePrefixOp TCharLength
  {-# INLINABLE octetLengthE #-}
  octetLengthE = makePrefixOp TOctetLength
  {-# INLINABLE bitLengthE #-}
  bitLengthE = makePrefixOp BBitLength
  {-# INLINABLE lowerE #-}
  lowerE = makePrefixOp TLower
  {-# INLINABLE upperE #-}
  upperE = makePrefixOp TUpper
  {-# INLINABLE trimE #-}
  trimE = makePrefixOp TTrim
  {-# INLINABLE absE #-}
  absE = makePrefixOp NAbs
  {-# INLINABLE extractE #-}
  extractE field' from' = Extract (getAnn from') field' from'
  {-# INLINABLE existsE #-}
  existsE sel = Exists sel.ann sel
  {-# INLINABLE uniqueE #-}
  uniqueE sel = Unique sel.ann sel
  {-# INLINABLE subqueryE #-}
  subqueryE sel = Subquery sel.ann sel
  {-# INLINABLE currentTimestampE #-}
  currentTimestampE = CurrentTimestamp (ExpressionAnn Impure mempty mempty)
  {-# INLINABLE defaultE #-}
  defaultE = Default (ExpressionAnn go mempty mempty)
    where
      -- Prior to MySQL 8.0.13, DEFAULTs could only be constant expressions,
      -- aside from CURRENT_TIMESTAMP. However, subsequent to that, they are
      -- allowed to be anything.
      --
      -- Given that we have no way of (easily) checking how a default is
      -- specified here, we assume that from 8.0.13 onwards, DEFAULT is impure.
      -- Otherwise, we specify it as pure. This is non-ideal, but given that
      -- pre-8.0.13 we have one special case (which we have no way of testing
      -- here), we either give up the possibility of _any_ implementation of
      -- insertRowReturning, or leave one special case to check.
      --
      -- While this looks terrifying, since it's calling into C, we're actually
      -- getting the value of a #define (thus, it's about as compile-time
      -- constant as it gets).
      --
      -- TODO: Ensure we check for the CURRENT_TIMESTAMP case in
      -- insertRowReturning for safety. - Koz
      go :: Purity
      go = if [C.pure| int { MYSQL_VERSION_ID } |] >= 80013
            then Impure
            else Pure
  {-# INLINABLE inE #-}
  inE e es = In ann' e esV
    where
      ann' :: ExpressionAnn
      ann' = foldl' (\acc e' -> acc <> getAnn e') (getAnn e) esV
      esV :: Vector MySQLExpressionSyntax
      esV = fromList es
  {-# INLINABLE betweenE #-}
  betweenE arg lo' hi' =
    Between (getAnn arg <> getAnn lo' <> getAnn hi') arg lo' hi'

instance IsSql92AggregationExpressionSyntax MySQLExpressionSyntax where
  type Sql92AggregationSetQuantifierSyntax MySQLExpressionSyntax =
    MySQLAggregationSetQuantifierSyntax
  {-# INLINABLE countAllE #-}
  countAllE = CountAll mempty
  {-# INLINABLE countE #-}
  countE = makeAggregation Count
  {-# INLINABLE avgE #-}
  avgE = makeAggregation Avg
  {-# INLINABLE sumE #-}
  sumE = makeAggregation Sum
  {-# INLINABLE minE #-}
  minE = makeAggregation Min
  {-# INLINABLE maxE #-}
  maxE = makeAggregation Max

data MySQLProjectionSyntax = ProjectExpressions {
    ann         :: !ExpressionAnn,
    projections :: {-# UNPACK #-} !(Vector (MySQLExpressionSyntax, Maybe Text))
  }
  deriving stock (Show, Eq)

instance IsSql92ProjectionSyntax MySQLProjectionSyntax where
  type Sql92ProjectionExpressionSyntax MySQLProjectionSyntax =
    MySQLExpressionSyntax
  {-# INLINABLE projExprs #-}
  projExprs es = ProjectExpressions go esV
    where
      go :: ExpressionAnn
      go = foldMap (getAnn . fst) esV
      esV :: Vector (MySQLExpressionSyntax, Maybe Text)
      esV = fromList es

data MySQLTableNameSyntax = TableName {
  schema :: !(Maybe Text),
  name   :: {-# UNPACK #-} !Text
  }
  deriving stock (Eq, Show)

instance IsSql92TableNameSyntax MySQLTableNameSyntax where
  {-# INLINABLE tableName #-}
  tableName = TableName

newtype TableRowExpression =
  TableRowExpression (Vector MySQLExpressionSyntax)
  deriving stock (Eq, Show)

data MySQLTableSourceSyntax =
  TableNamed !MySQLTableNameSyntax |
  TableFromSubSelect !MySQLSelectSyntax |
  TableFromValues {
    ann  :: !ExpressionAnn,
    rows :: {-# UNPACK #-} !(Vector TableRowExpression)
    }
  deriving stock (Eq, Show)

tableSourceAnn :: MySQLTableSourceSyntax -> ExpressionAnn
tableSourceAnn = \case
  TableNamed (TableName _ nam) ->
    ExpressionAnn Impure mempty (HS.singleton nam)
  TableFromSubSelect sel -> sel.ann
  TableFromValues ann' _ -> ann'

instance IsSql92TableSourceSyntax MySQLTableSourceSyntax where
  type Sql92TableSourceTableNameSyntax MySQLTableSourceSyntax =
    MySQLTableNameSyntax
  type Sql92TableSourceExpressionSyntax MySQLTableSourceSyntax =
    MySQLExpressionSyntax
  type Sql92TableSourceSelectSyntax MySQLTableSourceSyntax =
    MySQLSelectSyntax
  {-# INLINABLE tableNamed #-}
  tableNamed = TableNamed
  {-# INLINABLE tableFromSubSelect #-}
  tableFromSubSelect = TableFromSubSelect
  {-# INLINABLE tableFromValues #-}
  tableFromValues rows' =
    TableFromValues (foldMap (foldMap getAnn) rows')
                    (fromList . fmap (TableRowExpression . fromList) $ rows')

data TableHeader =
  Anonymous |
  TableNameOnly {-# UNPACK #-} !Text |
  TableAndColumns {
    name        :: {-# UNPACK #-} !Text,
    columnNames :: {-# UNPACK #-} !(Vector Text)
    }
  deriving stock (Eq, Show)

data MySQLFromSyntax =
  FromTable {
    tableSource :: !MySQLTableSourceSyntax,
    tableHeader :: !TableHeader
    } |
  InnerJoin {
    ann       :: !ExpressionAnn,
    leftArg   :: MySQLFromSyntax,
    rightArg  :: MySQLFromSyntax,
    condition :: !(Maybe MySQLExpressionSyntax)
    } |
  LeftJoin {
    ann       :: !ExpressionAnn,
    leftArg   :: MySQLFromSyntax,
    rightArg  :: MySQLFromSyntax,
    condition :: !(Maybe MySQLExpressionSyntax)
    } |
  RightJoin {
    ann       :: !ExpressionAnn,
    leftArg   :: MySQLFromSyntax,
    rightArg  :: MySQLFromSyntax,
    condition :: !(Maybe MySQLExpressionSyntax)
    }
  deriving stock (Eq, Show)

fromAnn :: MySQLFromSyntax -> ExpressionAnn
fromAnn fs = case fs of
  FromTable{} ->
    tableSourceAnn fs.tableSource <>
    (case fs.tableHeader of
      Anonymous             -> mempty
      TableNameOnly nam     -> ExpressionAnn Impure mempty (HS.singleton nam)
      TableAndColumns nam _ -> ExpressionAnn Impure mempty (HS.singleton nam))
  InnerJoin{} -> fs.ann
  LeftJoin{} -> fs.ann
  RightJoin{} -> fs.ann

instance IsSql92FromSyntax MySQLFromSyntax where
  type Sql92FromTableSourceSyntax MySQLFromSyntax =
    MySQLTableSourceSyntax
  type Sql92FromExpressionSyntax MySQLFromSyntax =
    MySQLExpressionSyntax
  {-# INLINABLE fromTable #-}
  fromTable tableSource' mHead = FromTable tableSource' $ case mHead of
    Nothing -> Anonymous
    Just (nam, mCols) -> case mCols of
      Nothing       -> TableNameOnly nam
      Just colNames -> TableAndColumns nam . fromList $ colNames
  {-# INLINABLE innerJoin #-}
  innerJoin lArg rArg cond =
    InnerJoin (joinAnn lArg rArg cond) lArg rArg cond
  {-# INLINABLE leftJoin #-}
  leftJoin lArg rArg cond =
    LeftJoin (joinAnn lArg rArg cond) lArg rArg cond
  {-# INLINABLE rightJoin #-}
  rightJoin lArg rArg cond =
    RightJoin (joinAnn lArg rArg cond) lArg rArg cond

data MySQLGroupingSyntax = GroupByExpressions {
  ann   :: !ExpressionAnn,
  exprs :: {-# UNPACK #-} !(Vector MySQLExpressionSyntax)
  }
  deriving stock (Eq, Show)

instance IsSql92GroupingSyntax MySQLGroupingSyntax where
  type Sql92GroupingExpressionSyntax MySQLGroupingSyntax =
    MySQLExpressionSyntax
  {-# INLINABLE groupByExpressions #-}
  groupByExpressions es =
    GroupByExpressions (foldMap getAnn es) (fromList es)

-- And finally...
data MySQLSelect = SelectStmt {
  ann       :: !ExpressionAnn,
  table     :: !MySQLSelectTableSyntax,
  orderings :: {-# UNPACK #-} !(Vector MySQLOrderingSyntax),
  limit     :: !(Maybe Integer),
  offset    :: !(Maybe Integer)
  }
  deriving stock (Eq, Show)

instance IsSql92SelectSyntax MySQLSelect where
  type Sql92SelectSelectTableSyntax MySQLSelect =
    MySQLSelectTableSyntax
  type Sql92SelectOrderingSyntax MySQLSelect =
    MySQLOrderingSyntax
  {-# INLINABLE selectStmt #-}
  selectStmt table' orderings' limit' offset' =
    SelectStmt ann' table' (fromList orderings') limit' offset'
    where
      ann' :: ExpressionAnn
      ann' =
        table'.ann <>
        foldMap (.ann) orderings'

-- Helpers

makeAggregation ::
  AggOp ->
  Maybe MySQLAggregationSetQuantifierSyntax ->
  MySQLExpressionSyntax ->
  MySQLExpressionSyntax
makeAggregation op quant e = Aggregation (getAnn e) op quant e

makeBinOp ::
  BinOp ->
  MySQLExpressionSyntax ->
  MySQLExpressionSyntax ->
  MySQLExpressionSyntax
makeBinOp op l r = BinaryOperation (getAnn l <> getAnn r) op l r

makeCompOp ::
  CompOp ->
  Maybe MySQLQuantifierSyntax ->
  MySQLExpressionSyntax ->
  MySQLExpressionSyntax ->
  MySQLExpressionSyntax
makeCompOp op mQuant l r =
  ComparisonOperation (getAnn l <> getAnn r) op mQuant l r

makePrefixOp ::
  PrefOp ->
  MySQLExpressionSyntax ->
  MySQLExpressionSyntax
makePrefixOp op e = PrefixOperation (getAnn e) op e

makePostfixOp ::
  PostOp ->
  MySQLExpressionSyntax ->
  MySQLExpressionSyntax
makePostfixOp op e = PostfixOperation (getAnn e) op e

joinAnn :: (Foldable t) =>
  MySQLFromSyntax -> MySQLFromSyntax -> t MySQLExpressionSyntax -> ExpressionAnn
joinAnn lArg rArg cond = fromAnn lArg <> fromAnn rArg <> foldMap getAnn cond

