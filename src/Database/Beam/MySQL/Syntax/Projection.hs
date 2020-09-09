{-# LANGUAGE TypeFamilies #-}

module Database.Beam.MySQL.Syntax.Projection where

import           Data.Text (Text)
import           Data.Vector (Vector, fromList)
import           Database.Beam.Backend.SQL (IsSql92ProjectionSyntax (..))
import           Database.Beam.MySQL.Syntax.Expression (ExpressionAnn,
                                                        MySQLExpressionSyntax,
                                                        getAnn)

data MySQLProjectionSyntax =
  ProjectExpressions
    !ExpressionAnn
    {-# UNPACK #-} !(Vector (MySQLExpressionSyntax, Maybe Text))
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
