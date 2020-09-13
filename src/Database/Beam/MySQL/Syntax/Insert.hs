-- Due to RDP plugin
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}
{-# LANGUAGE TypeFamilies #-}

module Database.Beam.MySQL.Syntax.Insert where

import           Data.Text (Text)
import           Data.Vector (Vector, fromList)
import           Database.Beam.Backend.SQL (IsSql92InsertSyntax (..),
                                            IsSql92InsertValuesSyntax (..))
import           Database.Beam.MySQL.Syntax.Select (ExpressionAnn,
                                                    MySQLExpressionSyntax,
                                                    MySQLSelect,
                                                    MySQLTableNameSyntax,
                                                    TableRowExpression (TableRowExpression),
                                                    getAnn)

data MySQLInsertValuesSyntax =
  InsertSQLExpressions {
    ann          :: !ExpressionAnn,
    rowsToInsert :: {-# UNPACK #-} !(Vector TableRowExpression)
    } |
  InsertFromSQL {
    ann          :: !ExpressionAnn,
    selectSource :: MySQLSelect
    }
  deriving stock (Eq, Show)

instance IsSql92InsertValuesSyntax MySQLInsertValuesSyntax where
  type Sql92InsertValuesExpressionSyntax MySQLInsertValuesSyntax =
    MySQLExpressionSyntax
  type Sql92InsertValuesSelectSyntax MySQLInsertValuesSyntax =
    MySQLSelect
  {-# INLINABLE insertSqlExpressions #-}
  insertSqlExpressions :: [[MySQLExpressionSyntax]] -> MySQLInsertValuesSyntax
  insertSqlExpressions rows =
    InsertSQLExpressions (foldMap (foldMap getAnn) rows)
                         (fromList . fmap (TableRowExpression . fromList) $ rows)
  {-# INLINABLE insertFromSql #-}
  insertFromSql :: MySQLSelect -> MySQLInsertValuesSyntax
  insertFromSql sel = InsertFromSQL sel.ann sel

data MySQLInsert = InsertStmt {
  ann          :: !ExpressionAnn,
  tableName    :: {-# UNPACK #-} !MySQLTableNameSyntax,
  columns      :: {-# UNPACK #-} !(Vector Text),
  insertValues :: MySQLInsertValuesSyntax
  }
  deriving stock (Eq, Show)

instance IsSql92InsertSyntax MySQLInsert where
  type Sql92InsertValuesSyntax MySQLInsert =
    MySQLInsertValuesSyntax
  type Sql92InsertTableNameSyntax MySQLInsert =
    MySQLTableNameSyntax
  {-# INLINABLE insertStmt #-}
  insertStmt ::
    MySQLTableNameSyntax ->
    [Text] ->
    MySQLInsertValuesSyntax ->
    MySQLInsert
  insertStmt tableName' columns' insertValues' =
    InsertStmt insertValues'.ann
               tableName'
               (fromList columns')
               insertValues'
