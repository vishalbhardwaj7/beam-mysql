-- Due to RDP plugin.
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}
{-# LANGUAGE TypeFamilies #-}

module Database.Beam.MySQL.Syntax.Select where

import           Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import           Data.Text (Text)
import           Data.Vector (Vector, fromList, singleton)
import           Database.Beam.Backend.SQL (IsSql92AggregationExpressionSyntax (..),
                                            IsSql92ExpressionSyntax (..),
                                            IsSql92FromSyntax (..),
                                            IsSql92GroupingSyntax (..),
                                            IsSql92OrderingSyntax (..),
                                            IsSql92ProjectionSyntax (..),
                                            IsSql92SelectSyntax (..),
                                            IsSql92SelectTableSyntax (..),
                                            IsSql92TableNameSyntax (..),
                                            IsSql92TableSourceSyntax (..),
                                            IsSql99ConcatExpressionSyntax (..))
import           Database.Beam.MySQL.Syntax.DataType (MySQLDataTypeSyntax)
import           Database.Beam.MySQL.Syntax.Misc (MySQLAggregationSetQuantifierSyntax,
                                                  MySQLExtractFieldSyntax,
                                                  MySQLFieldNameSyntax (QualifiedField, UnqualifiedField),
                                                  MySQLQuantifierSyntax)
import           Database.Beam.MySQL.Syntax.Value (MySQLValueSyntax)
import           Prelude hiding (map)

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
  ascOrdering :: MySQLExpressionSyntax -> MySQLOrderingSyntax
  ascOrdering e = AscOrdering (getAnn e) e
  {-# INLINABLE descOrdering #-}
  descOrdering :: MySQLExpressionSyntax -> MySQLOrderingSyntax
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
  UnionTablesAll {
    ann    :: !ExpressionAnn,
    lTable :: MySQLSelectTableSyntax,
    rTable :: MySQLSelectTableSyntax
    } |
  UnionTablesDistinct {
    ann    :: !ExpressionAnn,
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
    MySQLSelect
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
  selectTableStmt ::
    Maybe MySQLAggregationSetQuantifierSyntax ->
    MySQLProjectionSyntax ->
    Maybe MySQLFromSyntax ->
    Maybe MySQLExpressionSyntax ->
    Maybe MySQLGroupingSyntax ->
    Maybe MySQLExpressionSyntax ->
    MySQLSelectTableSyntax
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
  unionTables ::
    Bool ->
    MySQLSelectTableSyntax ->
    MySQLSelectTableSyntax ->
    MySQLSelectTableSyntax
  unionTables isAll lOp rOp = if isAll
    then UnionTablesAll (lOp.ann <> rOp.ann) lOp rOp
    else UnionTablesDistinct (lOp.ann <> rOp.ann) lOp rOp
  {-# INLINABLE intersectTables #-}
  intersectTables ::
    Bool ->
    MySQLSelectTableSyntax ->
    MySQLSelectTableSyntax ->
    MySQLSelectTableSyntax
  intersectTables _ lOp rOp =
    IntersectTables (lOp.ann <> rOp.ann) lOp rOp
  {-# INLINABLE exceptTable #-}
  exceptTable ::
    Bool ->
    MySQLSelectTableSyntax ->
    MySQLSelectTableSyntax ->
    MySQLSelectTableSyntax
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

fromValue :: MySQLValueSyntax -> ExpressionAnn
fromValue p = ExpressionAnn Pure (singleton p) mempty

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

data CaseBranch = CaseBranch {
  condition :: !MySQLExpressionSyntax,
  action    :: !MySQLExpressionSyntax
  }
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
    ann         :: !ExpressionAnn,
    cases       :: {-# UNPACK #-} !(Vector CaseBranch),
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
    select :: MySQLSelect
    } |
  Unique {
    ann    :: !ExpressionAnn,
    select :: MySQLSelect
    } |
  Subquery {
    ann    :: !ExpressionAnn,
    select :: !MySQLSelect
    } |
  CountAll {
    ann :: !ExpressionAnn
    } |
  Aggregation {
    ann           :: !ExpressionAnn,
    aggregationOp :: !AggOp,
    setQuantifier :: !(Maybe MySQLAggregationSetQuantifierSyntax),
    expr          :: MySQLExpressionSyntax
    } |
  Concat {
    ann   :: !ExpressionAnn,
    exprs :: {-# UNPACK #-} !(Vector MySQLExpressionSyntax)
    } |
  -- This is needed to support runInsertRowReturning. It is unannotated, because
  -- it's never constructed by beam.
  LastInsertId
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
    MySQLSelect
  {-# INLINABLE valueE #-}
  valueE :: MySQLValueSyntax -> MySQLExpressionSyntax
  valueE = Placeholder . fromValue
  {-# INLINABLE rowE #-}
  rowE :: [MySQLExpressionSyntax] -> MySQLExpressionSyntax
  rowE es = Row (foldMap getAnn es) (fromList es)
  {-# INLINABLE coalesceE #-}
  coalesceE :: [MySQLExpressionSyntax] -> MySQLExpressionSyntax
  coalesceE es = Coalesce (foldMap getAnn es) (fromList es)
  {-# INLINABLE caseE #-}
  caseE ::
    [(MySQLExpressionSyntax, MySQLExpressionSyntax)] ->
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax
  caseE cases' def =
    Case (foldMap go cases' <> getAnn def)
         (fromList . fmap (uncurry CaseBranch) $ cases')
         def
    where
      go ::
        (MySQLExpressionSyntax, MySQLExpressionSyntax) ->
        ExpressionAnn
      go (cond, act) = getAnn cond <> getAnn act
  {-# INLINABLE fieldE #-}
  fieldE :: MySQLFieldNameSyntax -> MySQLExpressionSyntax
  fieldE f = Field go f
    where
      go :: ExpressionAnn
      go = ExpressionAnn Impure mempty $ case f of
        QualifiedField nam _ -> HS.singleton nam
        UnqualifiedField _   -> mempty
  {-# INLINABLE andE #-}
  andE ::
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax
  andE = makeBinOp LAnd
  {-# INLINABLE orE #-}
  orE ::
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax
  orE = makeBinOp LOr
  {-# INLINABLE addE #-}
  addE ::
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax
  addE = makeBinOp NAdd
  {-# INLINABLE mulE #-}
  mulE ::
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax
  mulE = makeBinOp NMul
  {-# INLINABLE subE #-}
  subE ::
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax
  subE = makeBinOp NSub
  {-# INLINABLE divE #-}
  divE ::
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax
  divE = makeBinOp NDiv
  {-# INLINABLE likeE #-}
  likeE ::
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax
  likeE = makeBinOp GLike
  {-# INLINABLE modE #-}
  modE ::
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax
  modE = makeBinOp NMod
  {-# INLINABLE overlapsE #-}
  overlapsE ::
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax
  overlapsE = makeBinOp GOverlaps
  {-# INLINABLE nullIfE #-}
  nullIfE ::
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax
  nullIfE l r = NullIf (getAnn l <> getAnn r) l r
  {-# INLINABLE positionE #-}
  positionE ::
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax
  positionE needle' haystack' =
    Position (getAnn needle' <> getAnn haystack') needle' haystack'
  {-# INLINABLE eqE #-}
  eqE ::
    Maybe MySQLQuantifierSyntax ->
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax
  eqE = makeCompOp CEq
  {-# INLINABLE neqE #-}
  neqE ::
    Maybe MySQLQuantifierSyntax ->
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax
  neqE = makeCompOp CNe
  {-# INLINABLE ltE #-}
  ltE ::
    Maybe MySQLQuantifierSyntax ->
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax
  ltE = makeCompOp CLt
  {-# INLINABLE gtE #-}
  gtE ::
    Maybe MySQLQuantifierSyntax ->
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax
  gtE = makeCompOp CGt
  {-# INLINABLE leE #-}
  leE ::
    Maybe MySQLQuantifierSyntax ->
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax
  leE = makeCompOp CLe
  {-# INLINABLE geE #-}
  geE ::
    Maybe MySQLQuantifierSyntax ->
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax
  geE = makeCompOp CGe
  {-# INLINABLE castE #-}
  castE ::
    MySQLExpressionSyntax ->
    MySQLDataTypeSyntax ->
    MySQLExpressionSyntax
  castE e dt = Cast (getAnn e) dt e
  {-# INLINABLE notE #-}
  notE ::
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax
  notE = makePrefixOp LNot
  {-# INLINABLE negateE #-}
  negateE ::
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax
  negateE = makePrefixOp NNegate
  {-# INLINABLE isNullE #-}
  isNullE ::
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax
  isNullE = makePostfixOp GIsNull
  {-# INLINABLE isNotNullE #-}
  isNotNullE ::
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax
  isNotNullE = makePostfixOp GIsNotNull
  {-# INLINABLE isTrueE #-}
  isTrueE ::
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax
  isTrueE = makePostfixOp GIsTrue
  {-# INLINABLE isNotTrueE #-}
  isNotTrueE ::
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax
  isNotTrueE = makePostfixOp GIsNotTrue
  {-# INLINABLE isFalseE #-}
  isFalseE ::
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax
  isFalseE = makePostfixOp GIsFalse
  {-# INLINABLE isNotFalseE #-}
  isNotFalseE ::
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax
  isNotFalseE = makePostfixOp GIsNotFalse
  {-# INLINABLE isUnknownE #-}
  isUnknownE ::
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax
  isUnknownE = makePostfixOp GIsUnknown
  {-# INLINABLE isNotUnknownE #-}
  isNotUnknownE ::
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax
  isNotUnknownE = makePostfixOp GIsNotUnknown
  {-# INLINABLE charLengthE #-}
  charLengthE ::
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax
  charLengthE = makePrefixOp TCharLength
  {-# INLINABLE octetLengthE #-}
  octetLengthE ::
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax
  octetLengthE = makePrefixOp TOctetLength
  {-# INLINABLE bitLengthE #-}
  bitLengthE ::
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax
  bitLengthE = makePrefixOp BBitLength
  {-# INLINABLE lowerE #-}
  lowerE ::
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax
  lowerE = makePrefixOp TLower
  {-# INLINABLE upperE #-}
  upperE ::
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax
  upperE = makePrefixOp TUpper
  {-# INLINABLE trimE #-}
  trimE ::
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax
  trimE = makePrefixOp TTrim
  {-# INLINABLE absE #-}
  absE ::
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax
  absE = makePrefixOp NAbs
  {-# INLINABLE extractE #-}
  extractE ::
    MySQLExtractFieldSyntax ->
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax
  extractE field' from' = Extract (getAnn from') field' from'
  {-# INLINABLE existsE #-}
  existsE :: MySQLSelect -> MySQLExpressionSyntax
  existsE sel = Exists sel.ann sel
  {-# INLINABLE uniqueE #-}
  uniqueE :: MySQLSelect -> MySQLExpressionSyntax
  uniqueE sel = Unique sel.ann sel
  {-# INLINABLE subqueryE #-}
  subqueryE :: MySQLSelect -> MySQLExpressionSyntax
  subqueryE sel = Subquery sel.ann sel
  {-# INLINABLE currentTimestampE #-}
  currentTimestampE :: MySQLExpressionSyntax
  currentTimestampE = CurrentTimestamp (ExpressionAnn Impure mempty mempty)
  {-# INLINABLE defaultE #-}
  defaultE :: MySQLExpressionSyntax
  -- TODO: Check this ourselves in runInsertRowReturning. - Koz
  -- Note left below for posterity.
  defaultE = Default (ExpressionAnn Impure mempty mempty)
  {-
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
      --
      -- TODO: Perhaps it might be better to be impure always, then check our
      -- special case differently in insertRowReturning? - Koz
      go :: Purity
      go = if [C.pure| int { MYSQL_VERSION_ID } |] >= 80013
            then Impure
            else Pure
  -}
  {-# INLINABLE inE #-}
  inE ::
    MySQLExpressionSyntax ->
    [MySQLExpressionSyntax] ->
    MySQLExpressionSyntax
  inE e es = In (getAnn e <> foldMap getAnn es) e (fromList es)
  {-# INLINABLE betweenE #-}
  betweenE ::
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax
  betweenE arg lo' hi' =
    Between (getAnn arg <> getAnn lo' <> getAnn hi') arg lo' hi'

instance IsSql92AggregationExpressionSyntax MySQLExpressionSyntax where
  type Sql92AggregationSetQuantifierSyntax MySQLExpressionSyntax =
    MySQLAggregationSetQuantifierSyntax
  {-# INLINABLE countAllE #-}
  countAllE :: MySQLExpressionSyntax
  countAllE = CountAll mempty
  {-# INLINABLE countE #-}
  countE ::
    Maybe MySQLAggregationSetQuantifierSyntax ->
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax
  countE = makeAggregation Count
  {-# INLINABLE avgE #-}
  avgE ::
    Maybe MySQLAggregationSetQuantifierSyntax ->
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax
  avgE = makeAggregation Avg
  {-# INLINABLE sumE #-}
  sumE ::
    Maybe MySQLAggregationSetQuantifierSyntax ->
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax
  sumE = makeAggregation Sum
  {-# INLINABLE minE #-}
  minE ::
    Maybe MySQLAggregationSetQuantifierSyntax ->
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax
  minE = makeAggregation Min
  {-# INLINABLE maxE #-}
  maxE ::
    Maybe MySQLAggregationSetQuantifierSyntax ->
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax
  maxE = makeAggregation Max

instance IsSql99ConcatExpressionSyntax MySQLExpressionSyntax where
  {-# INLINABLE concatE #-}
  concatE es = Concat (foldMap getAnn es) (fromList es)

data Projection = Projection {
  expr  :: !MySQLExpressionSyntax,
  label :: !(Maybe Text)
  }
  deriving stock (Eq, Show)

data MySQLProjectionSyntax = ProjectExpressions {
    ann         :: !ExpressionAnn,
    projections :: {-# UNPACK #-} !(Vector Projection)
  }
  deriving stock (Show, Eq)

instance IsSql92ProjectionSyntax MySQLProjectionSyntax where
  type Sql92ProjectionExpressionSyntax MySQLProjectionSyntax =
    MySQLExpressionSyntax
  {-# INLINABLE projExprs #-}
  projExprs ::
    [(MySQLExpressionSyntax, Maybe Text)] ->
    MySQLProjectionSyntax
  projExprs es =
    ProjectExpressions (foldMap (getAnn . fst) es)
                       (fromList . fmap (uncurry Projection) $ es)

data MySQLTableNameSyntax = TableName {
  schema :: !(Maybe Text),
  name   :: {-# UNPACK #-} !Text
  }
  deriving stock (Eq, Show)

instance IsSql92TableNameSyntax MySQLTableNameSyntax where
  {-# INLINABLE tableName #-}
  tableName :: Maybe Text -> Text -> MySQLTableNameSyntax
  tableName = TableName

newtype TableRowExpression =
  TableRowExpression (Vector MySQLExpressionSyntax)
  deriving stock (Eq, Show)

data MySQLTableSourceSyntax =
  TableNamed !MySQLTableNameSyntax |
  TableFromSubSelect MySQLSelect |
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
    MySQLSelect
  {-# INLINABLE tableNamed #-}
  tableNamed :: MySQLTableNameSyntax -> MySQLTableSourceSyntax
  tableNamed = TableNamed
  {-# INLINABLE tableFromSubSelect #-}
  tableFromSubSelect :: MySQLSelect -> MySQLTableSourceSyntax
  tableFromSubSelect = TableFromSubSelect
  {-# INLINABLE tableFromValues #-}
  tableFromValues :: [[MySQLExpressionSyntax]] -> MySQLTableSourceSyntax
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
  fromTable ::
    MySQLTableSourceSyntax ->
    Maybe (Text, Maybe [Text]) ->
    MySQLFromSyntax
  fromTable tableSource' mHead = FromTable tableSource' $ case mHead of
    Nothing -> Anonymous
    Just (nam, mCols) -> case mCols of
      Nothing       -> TableNameOnly nam
      Just colNames -> TableAndColumns nam . fromList $ colNames
  {-# INLINABLE innerJoin #-}
  innerJoin ::
    MySQLFromSyntax ->
    MySQLFromSyntax ->
    Maybe MySQLExpressionSyntax ->
    MySQLFromSyntax
  innerJoin lArg rArg cond =
    InnerJoin (joinAnn lArg rArg cond) lArg rArg cond
  {-# INLINABLE leftJoin #-}
  leftJoin ::
    MySQLFromSyntax ->
    MySQLFromSyntax ->
    Maybe MySQLExpressionSyntax ->
    MySQLFromSyntax
  leftJoin lArg rArg cond =
    LeftJoin (joinAnn lArg rArg cond) lArg rArg cond
  {-# INLINABLE rightJoin #-}
  rightJoin ::
    MySQLFromSyntax ->
    MySQLFromSyntax ->
    Maybe MySQLExpressionSyntax ->
    MySQLFromSyntax
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
  groupByExpressions :: [MySQLExpressionSyntax] -> MySQLGroupingSyntax
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
  selectStmt ::
    MySQLSelectTableSyntax ->
    [MySQLOrderingSyntax] ->
    Maybe Integer ->
    Maybe Integer ->
    MySQLSelect
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

