{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies   #-}

module DBConfig (setUpDB, intoInsert, intoRawInsert, TestT(..), TestDB (..), testDb) where

import           Data.ByteString.Lazy (ByteString)
import           Data.Foldable (fold)
import           Data.List (intersperse)
import           Data.String (fromString)
import           Data.Text (Text, pack)
import           Database.Beam (Beamable, Columnar, Database, DatabaseSettings,
                                SqlInsert, Table (..), TableEntity,
                                defaultDbSettings, insert, insertValues)
import           Database.Beam.MySQL (MySQL)
import           Database.MySQL.Base (MySQLConn, Query (..), execute_)
import           GHC.Generics (Generic)

-- DO NOT use this outside of bracket!
setUpDB :: MySQLConn -> IO ()
setUpDB conn = do
  _ <- execute_ conn "create database test;"
  _ <- execute_ conn "use test"
  _ <- execute_ conn dbExpr
  pure ()
  where
    dbExpr = Query (
      "create table test_table (" <>
      "id int primary key, " <>
      (fold . intersperse ", " . fmap go $ [1 .. 37]) <>
      ");")
    go :: Int -> ByteString
    go i = "col" <>
           fromString (if i < 10 then "0" <> show i else show i) <>
           " varchar(50) not null"

intoRawInsert :: Int -> Query
intoRawInsert i = Query (
  "insert into test_table (id, "
  <>
  (fold . intersperse ", " . fmap go $ [1 .. 37]) <>
  ") values (" <>
  (fromString . show $ i) <>
  ", " <>
  (fold . intersperse ", " . replicate 37 $ constructed) <>
  ");")
  where
    constructed = "\"Test " <> (fromString . show $ i) <> "\""
    go :: Int -> ByteString
    go j = "col" <> fromString (if j < 10 then "0" <> show j else show j)

intoInsert :: Int -> SqlInsert MySQL TestT
intoInsert i = insert (_testTestTable testDb) . insertValues $ go
  where
    go = [TestT i
                ("Test " <> (pack . show $ i))
                ("Test " <> (pack . show $ i))
                ("Test " <> (pack . show $ i))
                ("Test " <> (pack . show $ i))
                ("Test " <> (pack . show $ i))
                ("Test " <> (pack . show $ i))
                ("Test " <> (pack . show $ i))
                ("Test " <> (pack . show $ i))
                ("Test " <> (pack . show $ i))
                ("Test " <> (pack . show $ i))
                ("Test " <> (pack . show $ i))
                ("Test " <> (pack . show $ i))
                ("Test " <> (pack . show $ i))
                ("Test " <> (pack . show $ i))
                ("Test " <> (pack . show $ i))
                ("Test " <> (pack . show $ i))
                ("Test " <> (pack . show $ i))
                ("Test " <> (pack . show $ i))
                ("Test " <> (pack . show $ i))
                ("Test " <> (pack . show $ i))
                ("Test " <> (pack . show $ i))
                ("Test " <> (pack . show $ i))
                ("Test " <> (pack . show $ i))
                ("Test " <> (pack . show $ i))
                ("Test " <> (pack . show $ i))
                ("Test " <> (pack . show $ i))
                ("Test " <> (pack . show $ i))
                ("Test " <> (pack . show $ i))
                ("Test " <> (pack . show $ i))
                ("Test " <> (pack . show $ i))
                ("Test " <> (pack . show $ i))
                ("Test " <> (pack . show $ i))
                ("Test " <> (pack . show $ i))
                ("Test " <> (pack . show $ i))
                ("Test " <> (pack . show $ i))
                ("Test " <> (pack . show $ i))
                ("Test " <> (pack . show $ i))]

-- Table
data TestT f =
  TestT
  { _testId    :: Columnar f Int,
    _testCol01 :: Columnar f Text,
    _testCol02 :: Columnar f Text,
    _testCol03 :: Columnar f Text,
    _testCol04 :: Columnar f Text,
    _testCol05 :: Columnar f Text,
    _testCol06 :: Columnar f Text,
    _testCol07 :: Columnar f Text,
    _testCol08 :: Columnar f Text,
    _testCol09 :: Columnar f Text,
    _testCol10 :: Columnar f Text,
    _testCol11 :: Columnar f Text,
    _testCol12 :: Columnar f Text,
    _testCol13 :: Columnar f Text,
    _testCol14 :: Columnar f Text,
    _testCol15 :: Columnar f Text,
    _testCol16 :: Columnar f Text,
    _testCol17 :: Columnar f Text,
    _testCol18 :: Columnar f Text,
    _testCol19 :: Columnar f Text,
    _testCol20 :: Columnar f Text,
    _testCol21 :: Columnar f Text,
    _testCol22 :: Columnar f Text,
    _testCol23 :: Columnar f Text,
    _testCol24 :: Columnar f Text,
    _testCol25 :: Columnar f Text,
    _testCol26 :: Columnar f Text,
    _testCol27 :: Columnar f Text,
    _testCol28 :: Columnar f Text,
    _testCol29 :: Columnar f Text,
    _testCol30 :: Columnar f Text,
    _testCol31 :: Columnar f Text,
    _testCol32 :: Columnar f Text,
    _testCol33 :: Columnar f Text,
    _testCol34 :: Columnar f Text,
    _testCol35 :: Columnar f Text,
    _testCol36 :: Columnar f Text,
    _testCol37 :: Columnar f Text
    }
    deriving stock (Generic)
    deriving anyclass (Beamable)

instance Table TestT where
  data PrimaryKey TestT f = TestTPK (Columnar f Int)
                            deriving stock (Generic)
                            deriving anyclass (Beamable)
  primaryKey = TestTPK . _testId

-- Database
newtype TestDB f =
  TestDB
    { _testTestTable :: f (TableEntity TestT)
    }
  deriving stock (Generic)
  deriving anyclass (Database MySQL)

testDb :: DatabaseSettings MySQL TestDB
testDb = defaultDbSettings
