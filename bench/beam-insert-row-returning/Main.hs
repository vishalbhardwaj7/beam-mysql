module Main (main) where

import           Control.Exception.Safe (bracket)
import           Data.Foldable (traverse_)
import           Database.Beam.MySQL (runBeamMySQL, runInsertRowReturning)
import           Database.MySQL.Base (close, connect)
import           Database.MySQL.Temp (toConnectInfo, withTempDB)
import           DBConfig (intoInsert, setUpDB)

main :: IO ()
main = withTempDB (\db -> do
        bracket (connect . toConnectInfo $ db)
                close
                go)
  where
    go conn = do
      setUpDB conn
      putStrLn ("Running " <> show nQueries <> " queries.")
      traverse_ (runBeamMySQL conn . runInsertRowReturning . intoInsert) [1 .. nQueries]

-- Helpers

nQueries :: Int
nQueries = 100000
