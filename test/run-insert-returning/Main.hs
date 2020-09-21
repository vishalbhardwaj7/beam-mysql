{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}

module Main (main) where

import           Control.Exception.Safe (bracket)
import           Data.Foldable (traverse_)
import           Data.Functor.Identity (Identity)
import           Data.Int (Int64)
import           Data.Kind (Type)
import           Data.Text (Text)
import           Database.Beam (Beamable, Columnar, Database, DatabaseSettings,
                                Table (PrimaryKey, primaryKey), TableEntity,
                                defaultDbSettings)
import           Database.Beam.MySQL (MySQL, runBeamMySQL,
                                      runInsertRowReturning)
import           Database.Beam.Query (QExpr, SqlInsert, default_, insert,
                                      insertExpressions, insertValues, val_)
import           Database.MySQL.Base (MySQLConn, Query (Query), close, connect,
                                      execute_)
import           Database.MySQL.Temp (toConnectInfo, withTempDB)
import           GHC.Generics (Generic)
import           Test.Hspec (SpecWith, beforeAll, describe, hspec, it, shouldBe)

main :: IO ()
main =
  withTempDB (\db -> bracket (connect . toConnectInfo $ db)
                              close
                              go)
  where
    go :: MySQLConn -> IO ()
    go conn = do
      setUpDB conn
      hspec $ do
        beforeAll (queryPkAi conn) pkAiSpec
        beforeAll (queryPkNoAi conn) pkNoAiSpec

-- Helpers

setUpDB :: MySQLConn -> IO ()
setUpDB conn = traverse_ (execute_ conn) [
  "create database test;",
  "use test",
  makePkAi,
  makePkNoAi
  ]
  where
    makePkAi :: Query
    makePkAi = Query $
      "create table pkai (" <>
      "id bigint primary key auto_increment, " <>
      "data varchar(255) not null);"
    makePkNoAi :: Query
    makePkNoAi = Query $
      "create table pknoai (" <>
      "id varchar(255) primary key, " <>
      "data varchar(255) not null);"

queryPkAi :: MySQLConn -> IO (Maybe (PkAiT Identity))
queryPkAi conn = runBeamMySQL conn . runInsertRowReturning $ go
  where
    go :: SqlInsert MySQL PkAiT
    go = insert (_testPkai testDB) (insertExpressions withDefault)
    withDefault :: forall s . [PkAiT (QExpr MySQL s)]
    withDefault = [PkAiT default_ (val_ "foo")]

queryPkNoAi :: MySQLConn -> IO (Maybe (PkNoAiT Identity))
queryPkNoAi conn = runBeamMySQL conn . runInsertRowReturning $ go
  where
    go :: SqlInsert MySQL PkNoAiT
    go = insert (_testPknoai testDB) (insertValues [PkNoAiT "foo" "bar"])

pkAiSpec :: SpecWith (Maybe (PkAiT Identity))
pkAiSpec = describe "Autoincrement primary key table inserts" $ do
  it "should return on success" $ \res -> do
    res `shouldBe` Just (PkAiT 1 "foo")

pkNoAiSpec :: SpecWith (Maybe (PkNoAiT Identity))
pkNoAiSpec = describe "Ordinary table inserts" $
  it "should return on success" $ \res -> do
    res `shouldBe` Just (PkNoAiT "foo" "bar")

-- Beam boilerplate

data PkAiT (f :: Type -> Type) = PkAiT
  {
    _pkaiId   :: Columnar f Int64,
    _pkaiData :: Columnar f Text
  }
  deriving stock (Generic)
  deriving anyclass (Beamable)

deriving stock instance Eq (PkAiT Identity)

deriving stock instance Show (PkAiT Identity)

instance Table PkAiT where
  data PrimaryKey PkAiT (f :: Type -> Type) =
    PkAiPK (Columnar f Int64)
    deriving stock (Generic)
    deriving anyclass (Beamable)
  primaryKey = PkAiPK . _pkaiId

data PkNoAiT (f :: Type -> Type) = PkNoAiT
  {
    _pknoaiId   :: Columnar f Text,
    _plnoaiData :: Columnar f Text
  }
  deriving stock (Generic)
  deriving anyclass (Beamable)

deriving stock instance Eq (PkNoAiT Identity)

deriving stock instance Show (PkNoAiT Identity)

instance Table PkNoAiT where
  data PrimaryKey PkNoAiT (f :: Type -> Type) =
    PkNoAiPK (Columnar f Text)
    deriving stock (Generic)
    deriving anyclass (Beamable)
  primaryKey = PkNoAiPK . _pknoaiId

data TestDB (f :: Type -> Type) = TestDB
  {
    _testPkai   :: f (TableEntity PkAiT),
    _testPknoai :: f (TableEntity PkNoAiT)
  }
  deriving stock (Generic)
  deriving anyclass (Database MySQL)

testDB :: DatabaseSettings MySQL TestDB
testDB = defaultDbSettings
