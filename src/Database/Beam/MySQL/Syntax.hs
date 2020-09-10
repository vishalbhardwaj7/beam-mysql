{-# LANGUAGE TypeFamilies #-}

module Database.Beam.MySQL.Syntax where

import           Database.Beam.Backend.SQL (IsSql92Syntax (..))
import           Database.Beam.MySQL.Syntax.Delete (MySQLDelete)
import           Database.Beam.MySQL.Syntax.Insert (MySQLInsert)
import           Database.Beam.MySQL.Syntax.Select (MySQLSelect)
import           Database.Beam.MySQL.Syntax.Update (MySQLUpdate)

data MySQLSyntax =
  ASelect MySQLSelect |
  AnInsert MySQLInsert |
  AnUpdate MySQLUpdate |
  ADelete MySQLDelete
  deriving stock (Eq, Show)

instance IsSql92Syntax MySQLSyntax where
  type Sql92SelectSyntax MySQLSyntax = MySQLSelect
  type Sql92UpdateSyntax MySQLSyntax = MySQLUpdate
  type Sql92DeleteSyntax MySQLSyntax = MySQLDelete
  type Sql92InsertSyntax MySQLSyntax = MySQLInsert
  {-# INLINABLE selectCmd #-}
  selectCmd = ASelect
  {-# INLINABLE insertCmd #-}
  insertCmd = AnInsert
  {-# INLINABLE updateCmd #-}
  updateCmd = AnUpdate
  {-# INLINABLE deleteCmd #-}
  deleteCmd = ADelete

{-
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
