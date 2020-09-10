module Main (main) where

import           Control.Exception.Safe (bracket)
import           Data.Foldable (traverse_)
import           Data.Text (Text)
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Database.MySQL.Base (ColumnDef, MySQLConn,
                                      MySQLValue (MySQLText), Query (Query),
                                      close, connect, execute_, queryVector_,
                                      skipToEof)
import           Database.MySQL.Temp (MySQLDB, toConnectInfo, withTempDB)
import           System.IO.Streams (InputStream)
import qualified System.IO.Streams as Stream
import           Test.Hspec (Spec, describe, hspec, it, shouldBe)

main :: IO ()
main = do
  res <- withTempDB go
  hspec . spec $ res
  where
    go :: MySQLDB -> IO Text
    go db = bracket (connect . toConnectInfo $ db)
                    close
                    (\conn -> setUpDB conn >>
                              runQuery conn)

-- Helpers

spec :: Text -> Spec
spec res = describe "Inserting Unicode into Latin-1 table" $ do
  it "should round-trip assuming UTF-8 connection" $ do
    res `shouldBe` "傍目八目"

setUpDB :: MySQLConn -> IO ()
setUpDB conn = traverse_ (execute_ conn) [
  "create database test;",
  "use test",
  makeTable,
  insertJapanese
  ]
  where
    makeTable :: Query
    makeTable = Query $
      "create table test_table (" <>
      "data varchar(255) not null primary key) " <>
      "default charset=latin1;"
    insertJapanese :: Query
    insertJapanese = Query
      "insert into test_table (data) values ('傍目八目');"

runQuery :: MySQLConn -> IO Text
runQuery conn = bracket (queryVector_ conn "select * from test_table;")
                        (skipToEof . snd)
                        go
  where
    go :: (Vector ColumnDef, InputStream (Vector MySQLValue)) -> IO Text
    go (_, stream) = do
      res <- Stream.read stream
      case (V.!? 0) =<< res of
        Nothing            -> fail "Query missed"
        Just (MySQLText t) -> pure t
        _                  -> fail "Unexpected result type."
