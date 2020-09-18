{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies   #-}

module Main (main) where

import           Data.Functor.Identity (Identity)
import           Data.Int (Int64)
import           Data.Kind (Type)
import           Data.Text (Text)
import           Database.Beam (Beamable, Columnar, Database, DatabaseSettings,
                                Table (PrimaryKey, primaryKey), TableEntity,
                                defaultDbSettings)
import           Database.Beam.MySQL (MySQL, dumpSelectSQL)
import           Database.Beam.Query (SqlSelect, all_, guard_, select, val_,
                                      (==.))
import           GHC.Generics (Generic)
import           Test.Hspec (describe, hspec, it, shouldBe)

main :: IO ()
main = hspec $ do
  describe "Simple selects" $ do
    it "should generate correctly without any extras" $
      dumpSelectSQL simpleSelect `shouldBe`
        Just ("SELECT `t0`.`id` AS `res0`, `t0`.`data` AS `res1` " <>
              "FROM `test_table` AS `t0`;")
    it "should generate correctly with WHERE" $
      dumpSelectSQL whereSelect `shouldBe`
        Just ("SELECT `t0`.`id` AS `res0`, `t0`.`data` AS `res1` " <>
              "FROM `test_table` AS `t0` " <>
              "WHERE (`t0`.`data`) = ('foo');")

-- Helpers

-- Beam boilerplate

data TestT (f :: Type -> Type) = TestT
  { _testId   :: Columnar f Int64,
    _testData :: Columnar f Text
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

whereSelect :: SqlSelect MySQL (TestT Identity)
whereSelect = select $ do
  res <- all_ . _testTestTable $ testDB
  guard_ (_testData res ==. val_ "foo")
  pure res
