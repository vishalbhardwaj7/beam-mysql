{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RankNTypes     #-}
{-# LANGUAGE TypeFamilies   #-}

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
import           Database.Beam.Query (QExpr, SqlInsertValues, default_, insert,
                                      insertExpressions, insertValues,
                                      runInsert, val_)
import           Database.MySQL.Base (MySQLConn, Query (Query), close, connect,
                                      execute_)
import           Database.MySQL.Temp (toConnectInfo, withTempDB)
import           GHC.Generics (Generic)
import           Test.Hspec (SpecWith, beforeAll, hspec)

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
        beforeAll (pkAiHit conn) pkAiHitSpec
        beforeAll (pkAiMiss conn) pkAiMissSpec
        -- beforeAll (pkNoAiHit conn) _
        -- beforeAll (pkNoAiMiss conn) _
        -- beforeAll (pkPlusAiHit conn) _
        -- beforeAll (pkPlusAiMiss conn) _

-- Helpers

setUpDB :: MySQLConn -> IO ()
setUpDB conn = traverse_ (execute_ conn) [
  "create database test;",
  "use test",
  makePkAi,
  makePkNoAi,
  makePkPlusAi
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
      "id bigint primary key, " <>
      "data varchar(255) not null);"
    makePkPlusAi :: Query
    makePkPlusAi = Query $
      "create table pkplusai (" <>
      "id varchar(255) primary key, " <>
      "incrementer bigint not null auto_increment, " <>
      "data varchar(255) not null);"

pkAiHit :: MySQLConn -> IO (Maybe (PkAiT Identity))
pkAiHit conn =
  runBeamMySQL conn . runInsertRowReturning . insert (_testPkai testDB) $ go
  where
    go :: SqlInsertValues MySQL (PkAiT (QExpr MySQL s))
    go = insertExpressions oneRow
    oneRow :: forall s' . [PkAiT (QExpr MySQL s')]
    oneRow = [PkAiT default_ (val_ "foo")]

pkAiMiss :: MySQLConn -> IO (Maybe (PkAiT Identity))
pkAiMiss conn = runBeamMySQL conn $ do
  runInsert . insert (_testPkai testDB) $ go
  runInsertRowReturning . insert (_testPkai testDB) $ go
  where
    go :: SqlInsertValues MySQL (PkAiT (QExpr MySQL s))
    go = insertValues [PkAiT 100 "bar"]

pkAiHitSpec :: SpecWith (Maybe (PkAiT Identity))
pkAiHitSpec = _

pkAiMissSpec :: SpecWith (Maybe (PkAiT Identity))
pkAiMissSpec = _

-- Beam boilerplate

data PkAiT (f :: Type -> Type) = PkAiT
  {
    _pkaiId   :: Columnar f Int64,
    _pkaiData :: Columnar f Text
  }
  deriving stock (Generic)
  deriving anyclass (Beamable)

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

instance Table PkNoAiT where
  data PrimaryKey PkNoAiT (f :: Type -> Type) =
    PkNoAiPK (Columnar f Text)
    deriving stock (Generic)
    deriving anyclass (Beamable)
  primaryKey = PkNoAiPK . _pknoaiId

data PkPlusAiT (f :: Type -> Type) = PkPlusAiT
  {
    _pkplusaiId          :: Columnar f Text,
    _pkplusaiIncrementer :: Columnar f Int64,
    _pkplusaiData        :: Columnar f Text
  }
  deriving stock (Generic)
  deriving anyclass (Beamable)

instance Table PkPlusAiT where
  data PrimaryKey PkPlusAiT (f :: Type -> Type) =
    PkPlusAiPK (Columnar f Text)
    deriving stock (Generic)
    deriving anyclass (Beamable)
  primaryKey = PkPlusAiPK . _pkplusaiId

data TestDB (f :: Type -> Type) = TestDB
  {
    _testPkai     :: f (TableEntity PkAiT),
    _testPknoai   :: f (TableEntity PkNoAiT),
    _testPkplusai :: f (TableEntity PkPlusAiT)
  }
  deriving stock (Generic)
  deriving anyclass (Database MySQL)

testDB :: DatabaseSettings MySQL TestDB
testDB = defaultDbSettings
