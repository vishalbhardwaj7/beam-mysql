{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies   #-}

module Main (main) where

import           Control.Exception.Safe (bracket)
import           Data.Foldable (traverse_)
import           Data.Text (Text, pack)
import           Database.Beam (Beamable, Columnar, Database, DatabaseSettings,
                                Table (..), TableEntity, defaultDbSettings,
                                insert, insertValues)
import           Database.Beam.MySQL (MySQL, runBeamMySQL,
                                      runInsertRowReturning)
import           Database.MySQL.Base (close, connect, execute_)
import           GHC.Generics (Generic)
import           TmpMySQL (toConnectInfo, withTempDB)

main :: IO ()
main = withTempDB (\db -> do
        bracket (connect . toConnectInfo $ db)
                close
                go)
  where
    go conn = do
      _ <- execute_ conn "create database test;"
      _ <- execute_ conn "use test"
      _ <- execute_ conn "create table test_table (name varchar(50) primary key, number_of_pets int unsigned not null);"
      putStrLn ("Running " <> show nQueries <> " queries.")
      traverse_ (runBeamMySQL conn . runInsertRowReturning . intoInsert) [1 .. nQueries]
    intoInsert i =
      insert (_testTestTable testDb) . insertValues $ [TestT ("Joe" <> (pack . show $ i)) i]

-- Helpers

nQueries :: Word
nQueries = 100000

-- Table
data TestT f =
  TestT
   { _testName         :: Columnar f Text,
     _testNumberOfPets :: Columnar f Word
   }
  deriving stock (Generic)
  deriving anyclass (Beamable)

instance Table TestT where
  data PrimaryKey TestT f = TestTPK (Columnar f Text)
                            deriving stock (Generic)
                            deriving anyclass (Beamable)
  primaryKey = TestTPK . _testName

-- Database
newtype TestDB f =
  TestDB
    { _testTestTable :: f (TableEntity TestT)
    }
  deriving stock (Generic)
  deriving anyclass (Database MySQL)

testDb :: DatabaseSettings MySQL TestDB
testDb = defaultDbSettings
