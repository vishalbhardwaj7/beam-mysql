{-# LANGUAGE DeriveAnyClass #-}

module Main (main) where

import           Control.Exception.Safe (bracket)
import qualified Data.ByteString as BS
import           Data.Foldable (traverse_)
import           Data.Functor.Identity (Identity)
import           Data.Int (Int64)
import           Data.Kind (Type)
import           Data.Text (Text)
import           Data.Text.Encoding (decodeLatin1)
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Database.Beam (Beamable, Columnar, Database, DatabaseSettings,
                                Table (PrimaryKey, primaryKey), TableEntity,
                                all_, asc_, defaultDbSettings, insert,
                                insertValues, orderBy_, runInsert,
                                runSelectReturningList, select)
import           Database.Beam.MySQL (MySQL, runBeamMySQL)
import           Database.Beam.Query (QExpr, SqlInsertValues)
import           Database.MySQL.Base (MySQLConn, Query (Query), close, connect,
                                      execute_)
import           Database.MySQL.Temp (MySQLDB, toConnectInfo, withTempDB)
import           GHC.Generics (Generic)
import           Test.Hspec (Spec, describe, hspec, it, shouldBe)

main :: IO ()
main = do
  res <- withTempDB go
  hspec . spec $ res
  where
    go :: MySQLDB -> IO (Vector Text)
    go db = bracket (connect . toConnectInfo $ db)
                    close
                    (\conn -> setUpDB conn >> insertSample conn >> retrieveSample conn)

-- Helpers

spec :: Vector Text -> Spec
spec v = describe "Latin-1-only valid round-tripping" $ do
  it "should round-trip properly" $ do
    v `shouldBe` sampleData

-- Every byte from 0x80 to 0xFF as a singleton string
sampleData :: Vector Text
sampleData = V.fromList . fmap (decodeLatin1 . BS.singleton) $ [0x80 .. 0xFF]

setUpDB :: MySQLConn -> IO ()
setUpDB conn = traverse_ (execute_ conn) [
  "create database test;",
  "use test",
  makeTestTable
  ]
  where
    makeTestTable :: Query
    makeTestTable = Query $
      "create table test_table (" <>
      "id bigint primary key not null, " <>
      "data varchar(255) not null);"

insertSample :: MySQLConn -> IO ()
insertSample conn = runBeamMySQL conn . runInsert . insert (_testTestTable testDB) $ go
  where
    go :: SqlInsertValues MySQL (TestT (QExpr MySQL s))
    go = insertValues . V.toList . V.imap buildTestT $ sampleData
    buildTestT :: Int -> Text -> TestT Identity
    buildTestT i t = TestT (fromIntegral i) t

retrieveSample :: MySQLConn -> IO (Vector Text)
retrieveSample conn =
  fmap V.fromList . runBeamMySQL conn . runSelectReturningList . select $ do
    res <- orderBy_ (asc_ . _testId) . all_ . _testTestTable $ testDB
    pure . _testData $ res

-- Beam boilerplate

data TestT (f :: Type -> Type) = TestT
  {
    _testId   :: Columnar f Int64,
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
