{-#LANGUAGE NamedFieldPuns #-}
module Main (main) where

import           Control.Exception.Safe (bracket, try)
import           DB (nullable, nullableMaybe, testDB)
import           DB.Nullable (NullableT)
import           DB.NullableMaybe (NullableMaybeT(..))
import           Data.Functor.Identity (Identity)
import qualified Data.HashSet as HS
import           Database.Beam (SqlSelect, all_, runSelectReturningOne, select)
import           Database.Beam.MySQL (ColumnDecodeError (FoundUnexpectedNull),
                                      MySQL, columnName, runBeamMySQL,
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
tests conn = testGroup "Unexpected NULL"
  [ testCase "erroring for Text" goNullable
  , testCase "passing for Maybe Text" goNullableMaybe
  ]
  where
    goNullable :: Assertion
    goNullable = do
      res <- runQueryCatching conn query
      case res of
        Left cde@FoundUnexpectedNull{} -> do
          assertBool "contains table name" (HS.member "nullable" . tablesInvolved $ cde)
          assertEqual "indicates correct column" "data" . columnName $ cde
        Left _ -> assertFailure "Did not indicate we got an unexpected NULL."
        Right _  -> assertFailure "Got a result when not expecting any."
    goNullableMaybe :: Assertion
    goNullableMaybe = do
      res <- runQueryCatchingMaybe conn queryMaybe
      case res of
        Left FoundUnexpectedNull{} -> assertFailure "Yay! Sandbox issue reproduced!"
        Left _ -> assertFailure "Yay! Sandbox issue probbaly reproduced!"
        Right (Just NullableMaybeT{dat}) -> assertEqual "NULL means Nothing" Nothing dat
        Right Nothing -> assertFailure "There should be one row"

runQueryCatching ::
  MySQLConn ->
  SqlSelect MySQL (NullableT Identity) ->
  IO (Either ColumnDecodeError (Maybe (NullableT Identity)))
runQueryCatching conn = try . runBeamMySQL conn . runSelectReturningOne

runQueryCatchingMaybe ::
  MySQLConn ->
  SqlSelect MySQL (NullableMaybeT Identity) ->
  IO (Either ColumnDecodeError (Maybe (NullableMaybeT Identity)))
runQueryCatchingMaybe conn = try . runBeamMySQL conn . runSelectReturningOne

query :: SqlSelect MySQL (NullableT Identity)
query = select . all_ . nullable $ testDB

queryMaybe :: SqlSelect MySQL (NullableMaybeT Identity)
queryMaybe = select . all_ . nullableMaybe $ testDB
