module Main (main) where

import           Control.Exception.Safe (bracket)
import           Data.Foldable (traverse_)
import           Data.String (fromString)
import           Database.MySQL.Base (Query (..), close, connect, execute_)
import           TmpMySQL (toConnectInfo, withTempDB)

nQueries :: Word
nQueries = 100000

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
      putStrLn $ "Running " <> show nQueries <> " queries"
      traverse_ (execute_ conn . insertStatement) [1 .. nQueries]
    insertStatement i = Query ("insert into test_table (name, number_of_pets) values (\"Joe" <>
                               (fromString . show $ i) <>
                               "\", " <>
                               (fromString . show $ i) <>
                               ");")
