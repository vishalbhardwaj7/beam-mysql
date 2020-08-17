{-# LANGUAGE ScopedTypeVariables #-}

module TmpMySQL where

import           Control.Concurrent (threadDelay)
import           Control.Exception.Safe (MonadMask, bracket)
import           Control.Monad.IO.Class (MonadIO (..))
import           Data.String (fromString)
import           Data.Word (Word16)
import           Database.MySQL.Base (ConnectInfo (..), defaultConnectInfoMB4)
import           System.Directory (createDirectory, getCurrentDirectory,
                                   getTemporaryDirectory, removePathForcibly,
                                   setCurrentDirectory)
import           System.Exit (ExitCode (..), exitWith)
import           System.FilePath ((</>))
import           System.Posix.User (getLoginName)
import           System.Process.Typed (shell, startProcess, stopProcess,
                                       waitExitCode)
import           System.Random (getStdRandom, random)

data MySQLDB = MySQLDB {
  port   :: {-# UNPACK #-} !Word16,
  user   :: !String,
  socket :: !FilePath
  }
  deriving stock (Eq)

toConnectInfo :: MySQLDB -> ConnectInfo
toConnectInfo db = defaultConnectInfoMB4 {
                    ciHost = "localhost",
                    ciPort = fromIntegral . port $ db,
                    ciDatabase = "",
                    ciUser = fromString . user $ db
                    }

withTempDbDir :: (MonadIO m, MonadMask m) => (FilePath -> m a) -> m a
withTempDbDir act = do
  rand :: Word <- liftIO . getStdRandom $ random
  tmp <- liftIO getTemporaryDirectory
  let tempDir = tmp </> ("mysql" <> show rand)
  bracket (liftIO . createDirectory $ tempDir)
          (\_ -> liftIO . removePathForcibly $ tempDir)
          (\_ -> act tempDir)

withTempDB :: (MonadIO m, MonadMask m) => (MySQLDB -> m a) -> m a
withTempDB act = withTempDbDir go
  where
    go tmpDir = do
      currDir <- liftIO getCurrentDirectory
      userName <- liftIO getLoginName
      let initDbCmd = shell ("mysqld --initialize-insecure --user=" <>
                              userName <>
                              " --datadir=" <>
                              tmpDir <>
                              " --explicit-defaults-for-timestamp")
      bracket (liftIO . setCurrentDirectory $ tmpDir)
              (\_ -> liftIO . setCurrentDirectory $ currDir)
              (\_ -> do
                  proc <- startProcess initDbCmd -- this will stop on its own
                  ec <- waitExitCode proc
                  case ec of
                    ExitSuccess             -> do
                      let sock = tmpDir </> "mysqld.sock"
                      let runDbCmd = shell ("mysqld --user=" <>
                                            userName <>
                                            " --datadir=" <>
                                            tmpDir <>
                                            " --socket=" <>
                                            sock)
                      bracket (startProcess runDbCmd)
                              stopProcess
                              (\_ -> do
                                liftIO . threadDelay $ 1000000
                                let db = MySQLDB 3306 "root" sock
                                act db)
                    failure@(ExitFailure _) -> liftIO . exitWith $ failure)

