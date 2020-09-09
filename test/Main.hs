{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies   #-}

module Main (main) where

import           Control.Exception.Safe (bracket)
import           Data.Foldable (traverse_)
import           Data.Functor.Identity (Identity)
import           Data.Int (Int16, Int32, Int64, Int8)
import           Data.Kind (Type)
import           Data.Text (Text)
import           Database.Beam (Beamable, Columnar, Database, DatabaseSettings,
                                Table (PrimaryKey, primaryKey), TableEntity,
                                defaultDbSettings, runSelectReturningOne)
import           Database.Beam.MySQL (MySQL, runBeamMySQL)
import           Database.Beam.Query (SqlSelect, all_, select)
import           Database.MySQL.Base (MySQLConn, Query (Query), close, connect,
                                      execute_)
import           Database.MySQL.Temp (MySQLDB, toConnectInfo, withTempDB)
import           GHC.Generics (Generic)
import           Test.Hspec (Spec, describe, hspec, it, shouldBe)

import           Data.ByteString as ByteString
import qualified Data.Text.Lazy as TL

import qualified Data.ByteString.Lazy as BSL

import qualified Database.Beam as B

main :: IO ()
main = do
  res <- withTempDB go
  hspec . spec $ res
  where
    go :: MySQLDB -> IO (CorrectT Identity)
    go db = bracket (connect . toConnectInfo $ db)
                    close
                    (\conn -> setUpDB conn >>
                              runCorrectQuery conn)

-- Helpers

setUpDB :: MySQLConn -> IO ()
setUpDB conn = traverse_ (execute_ conn) [
  "create database test;",
  "use test",
  makeCorrect,
  insertCorrect
  ]
  where
    makeCorrect :: Query
    makeCorrect = Query $
      "create table correct_table (" <>
      "id bigint primary key auto_increment, " <>
      "bad_text varchar(255) not null, " <>
      "bad_text2 text not null" <>
      ");"
    insertCorrect :: Query
    insertCorrect = Query $
      "insert into correct_table (" <>
      "id, bad_text, bad_text2)"  <>
      "values (" <>
      "DEFAULT," <>
      "\'foo\\'\'," <>
      bobbyTablesBS <>
      ")"

runCorrectQuery :: MySQLConn -> IO (CorrectT Identity)
runCorrectQuery conn = do
  res <- runBeamMySQL conn . B.runSelectReturningList $ query
  case res of
    []   -> fail "Expected query result, but got nothing."
    res':_ -> do
      -- print res
      print $ _correctBadText res'
      print $ _correctBadText2 res'
      pure $ res'
  where
    query :: SqlSelect MySQL (CorrectT Identity)
    query = select . B.limit_ 1 . B.filter_' predicate $ B.all_ (_testCorrectTable testDB)
      where predicate CorrectT {_correctBadText2} =
              _correctBadText2 B.==?. B.val_ bobbyTables

bobbyTables :: Text
bobbyTables = "\'; DROP TABLE students; --"

bobbyTablesBS :: BSL.ByteString
bobbyTablesBS = "\'\\'; DROP TABLE students; --\'"

spec :: CorrectT Identity -> Spec
spec res = do
  describe "Escaping tests" $ do
    it "shouldn't allow little Bobby Tables to drop students table" $ do
      _correctBadText2 res `shouldBe` bobbyTables

data CorrectT (f :: Type -> Type) = CorrectT
  {
    _correctId       :: Columnar f Int64,
    _correctBadText  :: Columnar f Text,
    _correctBadText2 :: Columnar f Text
  }
  deriving stock (Generic)
  deriving anyclass (Beamable)

instance Table CorrectT where
  data PrimaryKey CorrectT (f :: Type -> Type) =
    CorrectTPK (Columnar f Int64)
    deriving stock (Generic)
    deriving anyclass (Beamable)
  primaryKey = CorrectTPK . _correctId

newtype TestDB (f :: Type -> Type) = TestDB
  {
    _testCorrectTable :: f (TableEntity CorrectT)
  }
  deriving stock (Generic)
  deriving anyclass (Database MySQL)

testDB :: DatabaseSettings MySQL TestDB
testDB = defaultDbSettings
