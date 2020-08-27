module Main (main) where

import           Control.Exception.Safe (bracket)
import           Data.Foldable (traverse_)
import           Database.MySQL.Base (close, connect, execute_)
import           Database.MySQL.Temp (toConnectInfo, withTempDB)
import           DBConfig (intoRawInsert, setUpDB)

main :: IO ()
main = withTempDB (\db -> do
    bracket (connect . toConnectInfo $ db)
            close
            go)
  where
    go conn = do
      setUpDB conn
      putStrLn $ "Running " <> show nQueries <> " queries"
      traverse_ (execute_ conn . intoRawInsert) [1 .. nQueries]

-- Helpers

nQueries :: Int
nQueries = 100000
