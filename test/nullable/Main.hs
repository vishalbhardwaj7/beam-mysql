module Main (main) where

import           Control.Exception.Safe (bracket, try)
import           DB (nullable, testDB)
import           DB.Nullable (NullableT)
import           Data.Functor.Identity (Identity)
import qualified Data.HashSet as HS
import           Database.Beam (SqlSelect, all_, runSelectReturningOne, select)
import           Database.Beam.MySQL (ColumnDecodeError (FoundUnexpectedNull),
                                      MySQL, columnIndex, runBeamMySQL,
                                      tablesInvolved)
import           Database.MySQL.Base (MySQLConn, ciCharset, ciDatabase, ciHost,
                                      ciPort, ciUser, close, connect,
                                      defaultConnectInfoMB4)
import           Test.Tasty (TestTree, defaultMain, testGroup)
import           Test.Tasty.HUnit (Assertion, assertBool, assertEqual,
                                   assertFailure, testCase)

main :: IO ()
main = bracket mkConnection close (defaultMain . tests)

-- Helpers

mkConnection :: IO MySQLConn
mkConnection = do
  let connInfo = defaultConnectInfoMB4 {
                    ciHost = "localhost",
                    ciPort = 3306,
                    ciDatabase = "test",
                    ciUser = "root",
                    ciCharset = 8
                    }
  connect connInfo

tests :: MySQLConn -> TestTree
tests conn = testGroup "Unexpected NULL" [
  testCase "erroring" go
  ]
  where
    go :: Assertion
    go = do
      res <- runQueryCatching conn query
      case res of
        Left cde@FoundUnexpectedNull{} -> do
          assertBool "contains table name" (HS.member "nullable" . tablesInvolved $ cde)
          assertEqual "indicates correct column" 1 (columnIndex cde)
        Left _ -> assertFailure "Did not indicate we got an unexpected NULL."
        Right _  -> assertFailure "Got a result when not expecting any."

runQueryCatching ::
  MySQLConn ->
  SqlSelect MySQL (NullableT Identity) ->
  IO (Either ColumnDecodeError (Maybe (NullableT Identity)))
runQueryCatching conn = try . runBeamMySQL conn . runSelectReturningOne

query :: SqlSelect MySQL (NullableT Identity)
query = select . all_ . nullable $ testDB
