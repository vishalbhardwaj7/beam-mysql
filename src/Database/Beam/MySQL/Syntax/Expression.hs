{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

module Database.Beam.MySQL.Syntax.Expression where

import           Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import           Data.Text (Text)
import           Data.Vector (Vector, foldl', foldl1', fromList, map, singleton)
import           Database.Beam.Backend.SQL (IsSql92ExpressionSyntax (..))
import           Database.Beam.MySQL.Syntax.DataType (MySQLDataTypeSyntax (..))
import           Database.Beam.MySQL.Syntax.Misc (MySQLExtractFieldSyntax, MySQLFieldNameSyntax (QualifiedField, UnqualifiedField),
                                                  MySQLQuantifierSyntax)
import           Database.Beam.MySQL.Syntax.Value (MySQLValueSyntax)
import qualified Language.C.Inline as C
import           Prelude hiding (map)

C.include "<mysql/mysql.h>"

-- TODO: Stub
data MySQLSelectSyntax = MySQLSelectSyntax
  deriving stock (Eq, Show)

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
    f@(QualifiedField tableName _) ->
      Field (ExpressionAnn Impure mempty (HS.singleton tableName)) f
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
  extractE field' from = Extract (getAnn from) field' from
  {-# INLINABLE existsE #-}
  existsE sel = Exists (ExpressionAnn Impure mempty mempty) sel -- TODO: Complete.
  {-# INLINABLE uniqueE #-}
  uniqueE sel = Unique (ExpressionAnn Impure mempty mempty) sel -- TODO: Complete.
  {-# INLINABLE subqueryE #-}
  subqueryE sel = Subquery (ExpressionAnn Impure mempty mempty) sel -- TODO: Complete.
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

-- Helpers

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
