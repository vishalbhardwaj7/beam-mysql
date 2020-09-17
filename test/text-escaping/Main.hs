{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RankNTypes     #-}
{-# LANGUAGE TypeFamilies   #-}

module Main (main) where

import           Data.Functor.Identity (Identity)
import           Data.Kind (Type)
import           Data.Text (Text)
import           Database.Beam (Beamable, Columnar, Database, DatabaseSettings,
                                Table (PrimaryKey, primaryKey), TableEntity,
                                defaultDbSettings)
import           Database.Beam.MySQL (MySQL, dumpDeleteSQL, dumpInsertSQL,
                                      dumpSelectSQL, dumpUpdateSQL)
import           Database.Beam.Query (QBaseScope, QExpr, QFieldAssignment,
                                      SqlDelete, SqlInsert, SqlSelect,
                                      SqlUpdate, all_, delete, filter_, insert,
                                      insertValues, select, toNewValue,
                                      updateTable, val_, (==.))
import           GHC.Generics (Generic)
import           Test.Hspec (describe, hspec, it, shouldBe)

main :: IO ()
main = hspec $ do
  describe "'\\' in generated SQL" $ do
    it "should be escaped in INSERT VALUES literals" $
      dumpInsertSQL insertStmt `shouldBe`
        Just "INSERT INTO `test_table`(text) VALUES ('foo\\\"');"
    it "should be escaped in SELECT WHERE literals" $
      dumpSelectSQL selectWhereStmt `shouldBe`
        Just "SELECT `t0`.`text` AS `res0` FROM `test_table` AS `t0` WHERE (`t0`.`text`) = ('foo\\\"');"
    it "should be escaped in UPDATE WHERE literals" $
      dumpUpdateSQL updateWhereStmt `shouldBe`
        Just "UPDATE `test_table` SET `text`='bar\\\"' WHERE (`text`) = ('foo\\\"');"
    it "should be escaped in DELETE WHERE literals" $
      dumpDeleteSQL deleteWhereStmt `shouldBe`
        Just "DELETE FROM `test_table` WHERE (`text`) = ('foo\\\"');"

-- Helpers

insertStmt :: SqlInsert MySQL TestT
insertStmt =
  insert (_testTestTable testDB) . insertValues $ [
    TestT "foo\""
    ]

selectWhereStmt :: SqlSelect MySQL (TestT Identity)
selectWhereStmt = select . filter_ go . all_ . _testTestTable $ testDB
  where
    go :: TestT (QExpr MySQL QBaseScope) -> QExpr MySQL QBaseScope Bool
    go row = _testText row ==. val_ "foo\""

updateWhereStmt :: SqlUpdate MySQL TestT
updateWhereStmt = updateTable (_testTestTable testDB) upd wher
  where
    upd :: TestT (QFieldAssignment MySQL TestT)
    upd = TestT (toNewValue (val_ "bar\""))
    wher :: forall s . TestT (QExpr MySQL s) -> QExpr MySQL s Bool
    wher row = _testText row ==. val_ "foo\""

deleteWhereStmt :: SqlDelete MySQL TestT
deleteWhereStmt = delete (_testTestTable testDB) go
  where
    go :: (forall s' . TestT (QExpr MySQL s')) -> QExpr MySQL s Bool
    go row = _testText row ==. val_ "foo\""

newtype TestT (f :: Type -> Type) = TestT
  {
    _testText :: Columnar f Text
  }
  deriving stock (Generic)
  deriving anyclass (Beamable)

instance Table TestT where
  data PrimaryKey TestT (f :: Type -> Type) =
    TestTPK (Columnar f Text)
    deriving stock (Generic)
    deriving anyclass (Beamable)
  primaryKey = TestTPK . _testText

newtype TestDB (f :: Type -> Type) = TestDB
  {
    _testTestTable :: f (TableEntity TestT)
  }
  deriving stock (Generic)
  deriving anyclass (Database MySQL)

testDB :: DatabaseSettings MySQL TestDB
testDB = defaultDbSettings
