{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RankNTypes     #-}
{-# LANGUAGE TypeFamilies   #-}

module Main (main) where

import           Data.Functor.Identity (Identity)
import           Data.Int (Int64)
import           Data.Kind (Type)
import           Data.Text (Text)
import           Database.Beam (Beamable, Columnar, Database, DatabaseSettings,
                                Table (PrimaryKey, primaryKey), TableEntity,
                                defaultDbSettings)
import           Database.Beam.MySQL (MySQL, dumpInsertSQL, dumpSelectSQL)
import           Database.Beam.Query (QExpr, SqlInsert, SqlSelect, all_,
                                      default_, guard_, insert,
                                      insertExpressions, isNothing_, select,
                                      val_, (==.))
import           GHC.Generics (Generic)
import           Test.Tasty (TestTree, defaultMain, testGroup)
import           Test.Tasty.HUnit (Assertion, assertEqual, testCase)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "SQL generation" [
  simpleSelectTests,
  operatorSelectTests,
  insertTests
  ]

simpleSelectTests :: TestTree
simpleSelectTests = testGroup "simple SELECTs" [
  testCase "no extras" noExtras,
  testCase "with WHERE" withWhere
  ]
  where
    noExtras :: Assertion
    noExtras = assertEqual "" simpleSelectSQL . dumpSelectSQL $ simpleSelect
    withWhere :: Assertion
    withWhere = assertEqual "" whereSelectSQL . dumpSelectSQL $ whereSelect

operatorSelectTests :: TestTree
operatorSelectTests = testGroup "SELECTs with operators" [
  testCase "with IS NULL" withIsNull
  ]
  where
    withIsNull :: Assertion
    withIsNull = assertEqual "" isNullSelectSQL . dumpSelectSQL $ isNullSelect

insertTests :: TestTree
insertTests = testGroup "INSERTs" [
  testCase "VALUES" values,
  testCase "multi-value" multiValue
  ]
  where
    values :: Assertion
    values = assertEqual "" simpleInsertSQL . dumpInsertSQL $ simpleInsert
    multiValue :: Assertion
    multiValue = assertEqual "" multiInsertSQL . dumpInsertSQL $ multiInsert

-- Helpers

-- Beam boilerplate

data TestT (f :: Type -> Type) = TestT
  { _testId   :: Columnar f Int64,
    _testData :: Columnar f (Maybe Text)
  }
  deriving stock (Generic)
  deriving anyclass (Beamable)

instance Table TestT where
  data PrimaryKey TestT (f :: Type -> Type) =
    TestTPK (Columnar f Int64)
    deriving stock (Generic)
    deriving anyclass (Beamable)
  primaryKey = TestTPK . _testId

newtype TestDB (f :: Type -> Type) = TestDB
  {
    _testTestTable :: f (TableEntity TestT)
  }
  deriving stock (Generic)
  deriving anyclass (Database MySQL)

testDB :: DatabaseSettings MySQL TestDB
testDB = defaultDbSettings

-- Selects

simpleSelect :: SqlSelect MySQL (TestT Identity)
simpleSelect = select . all_ . _testTestTable $ testDB

simpleSelectSQL :: Maybe Text
simpleSelectSQL = Just $
  "SELECT `t0`.`id` AS `res0`, `t0`.`data` AS `res1` " <>
  "FROM `test_table` AS `t0`;"

whereSelect :: SqlSelect MySQL (TestT Identity)
whereSelect = select $ do
  res <- all_ . _testTestTable $ testDB
  guard_ (_testData res ==. val_ (Just "foo"))
  pure res

whereSelectSQL :: Maybe Text
whereSelectSQL = Just $
  "SELECT `t0`.`id` AS `res0`, `t0`.`data` AS `res1` " <>
  "FROM `test_table` AS `t0` " <>
  "WHERE CASE WHEN ((`t0`.`data`) IS NULL) AND (('foo') IS NULL) " <>
  "THEN TRUE " <>
  "WHEN ((`t0`.`data`) IS NULL) OR (('foo') IS NULL) " <>
  "THEN FALSE ELSE (`t0`.`data`) = ('foo') END;"

isNullSelect :: SqlSelect MySQL (TestT Identity)
isNullSelect = select $ do
  res <- all_ . _testTestTable $ testDB
  guard_ (isNothing_ . _testData $ res)
  pure res

isNullSelectSQL :: Maybe Text
isNullSelectSQL = Just $
  "SELECT `t0`.`id` AS `res0`, `t0`.`data` AS `res1` " <>
  "FROM `test_table` AS `t0` " <>
  "WHERE (`t0`.`data`) IS NULL;"

simpleInsert :: SqlInsert MySQL TestT
simpleInsert =
  insert (_testTestTable testDB) (insertExpressions [go])
  where
    go :: forall s . TestT (QExpr MySQL s)
    go = TestT default_ (val_ . Just $ "foo")

simpleInsertSQL :: Maybe Text
simpleInsertSQL = Just "INSERT INTO `test_table` (id, data) VALUES (DEFAULT, 'foo');"

multiInsert :: SqlInsert MySQL TestT
multiInsert =
  insert (_testTestTable testDB) (insertExpressions go)
  where
    go :: forall s . [TestT (QExpr MySQL s)]
    go = [
      TestT default_ (val_ . Just $ "foo"),
      TestT default_ (val_ . Just $ "bar")
      ]

multiInsertSQL :: Maybe Text
multiInsertSQL = Just "INSERT INTO `test_table` (id, data) VALUES (DEFAULT, 'foo'), (DEFAULT, 'bar');"
