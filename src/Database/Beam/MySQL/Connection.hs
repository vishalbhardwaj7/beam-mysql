{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Database.Beam.MySQL.Connection (
  MySQL(..),
  MySQLM(..),
  NotEnoughColumns (..),
  CouldNotReadColumn (..),
  runBeamMySQL, runBeamMySQLDebug
  ) where

import           Control.Exception.Safe (Exception (..), MonadCatch, MonadMask,
                                         MonadThrow, bracket, throwIO)
import           Control.Monad.Except (Except, catchError, runExcept,
                                       throwError)
import           Control.Monad.Free.Church (iterM)
import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Reader (ReaderT (..))
import           Control.Monad.State.Strict (StateT, evalStateT, get, gets,
                                             modify, put)
import           Data.Aeson (Value)
import           Data.ByteString (ByteString)
import           Data.Int (Int16, Int32, Int64, Int8)
import           Data.Scientific (Scientific)
import           Data.Text (Text)
import           Data.Time (Day, LocalTime, NominalDiffTime, TimeOfDay)
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Data.Word (Word16, Word32, Word64, Word8)
import           Database.Beam.Backend (BeamBackend (..), BeamRowReadError (..),
                                        BeamSqlBackend, BeamSqlBackendSyntax,
                                        FromBackendRow (..),
                                        FromBackendRowF (..),
                                        FromBackendRowM (..), MonadBeam (..))
import           Database.Beam.Backend.SQL (BeamSqlBackendIsString, SqlNull)
import           Database.Beam.MySQL.FromField (FromField (..))
import           Database.Beam.MySQL.Syntax (MysqlSyntax, intoDebugText,
                                             intoQuery)
import           Database.Beam.Query (HasQBuilder (..), HasSqlEqualityCheck,
                                      HasSqlQuantifiedEqualityCheck)
import           Database.Beam.Query.SQL92 (buildSql92Query')
import           Database.MySQL.Base (FieldType, MySQLConn, MySQLValue,
                                      columnType, execute_, queryVector_,
                                      skipToEof)
import           Prelude hiding (mapM, read)
import           System.IO.Streams (peek, read)
import           System.IO.Streams.Combinators (mapM)
import           System.IO.Streams.List (toList)

data MySQL = MySQL

instance BeamSqlBackendIsString MySQL String

instance BeamSqlBackendIsString MySQL Text

instance BeamBackend MySQL where
  type BackendFromField MySQL = FromField

instance BeamSqlBackend MySQL

type instance BeamSqlBackendSyntax MySQL = MysqlSyntax

instance HasQBuilder MySQL where
  buildSqlQuery = buildSql92Query' True

-- Instances for all types we can read using this library from the database

instance FromBackendRow MySQL Bool

instance HasSqlEqualityCheck MySQL Bool

instance HasSqlQuantifiedEqualityCheck MySQL Bool

instance FromBackendRow MySQL Int8

instance HasSqlEqualityCheck MySQL Int8

instance HasSqlQuantifiedEqualityCheck MySQL Int8

instance FromBackendRow MySQL Int16

instance HasSqlEqualityCheck MySQL Int16

instance HasSqlQuantifiedEqualityCheck MySQL Int16

instance FromBackendRow MySQL Int32

instance HasSqlEqualityCheck MySQL Int32

instance HasSqlQuantifiedEqualityCheck MySQL Int32

instance FromBackendRow MySQL Int64

instance HasSqlEqualityCheck MySQL Int64

instance HasSqlQuantifiedEqualityCheck MySQL Int64

instance FromBackendRow MySQL Int

instance HasSqlEqualityCheck MySQL Int

instance HasSqlQuantifiedEqualityCheck MySQL Int

instance FromBackendRow MySQL Word8

instance HasSqlEqualityCheck MySQL Word8

instance HasSqlQuantifiedEqualityCheck MySQL Word8

instance FromBackendRow MySQL Word16

instance HasSqlEqualityCheck MySQL Word16

instance HasSqlQuantifiedEqualityCheck MySQL Word16

instance FromBackendRow MySQL Word32

instance HasSqlEqualityCheck MySQL Word32

instance HasSqlQuantifiedEqualityCheck MySQL Word32

instance FromBackendRow MySQL Word64

instance HasSqlEqualityCheck MySQL Word64

instance HasSqlQuantifiedEqualityCheck MySQL Word64

instance FromBackendRow MySQL Word

instance HasSqlEqualityCheck MySQL Word

instance HasSqlQuantifiedEqualityCheck MySQL Word

instance FromBackendRow MySQL Float

instance HasSqlEqualityCheck MySQL Float

instance HasSqlQuantifiedEqualityCheck MySQL Float

instance FromBackendRow MySQL Double

instance HasSqlEqualityCheck MySQL Double

instance HasSqlQuantifiedEqualityCheck MySQL Double

instance FromBackendRow MySQL Scientific

instance HasSqlEqualityCheck MySQL Scientific

instance HasSqlQuantifiedEqualityCheck MySQL Scientific

instance FromBackendRow MySQL SqlNull

instance HasSqlEqualityCheck MySQL SqlNull

instance HasSqlQuantifiedEqualityCheck MySQL SqlNull

instance FromBackendRow MySQL ByteString

instance HasSqlEqualityCheck MySQL ByteString

instance HasSqlQuantifiedEqualityCheck MySQL ByteString

instance FromBackendRow MySQL Text

instance HasSqlEqualityCheck MySQL Text

instance HasSqlQuantifiedEqualityCheck MySQL Text

instance FromBackendRow MySQL LocalTime

instance HasSqlEqualityCheck MySQL LocalTime

instance HasSqlQuantifiedEqualityCheck MySQL LocalTime

instance FromBackendRow MySQL Day

instance HasSqlEqualityCheck MySQL Day

instance HasSqlQuantifiedEqualityCheck MySQL Day

instance FromBackendRow MySQL TimeOfDay

instance HasSqlEqualityCheck MySQL TimeOfDay

instance HasSqlQuantifiedEqualityCheck MySQL TimeOfDay

instance FromBackendRow MySQL NominalDiffTime

instance HasSqlEqualityCheck MySQL NominalDiffTime

instance HasSqlQuantifiedEqualityCheck MySQL NominalDiffTime

instance FromBackendRow MySQL Value

instance HasSqlEqualityCheck MySQL Value

instance HasSqlQuantifiedEqualityCheck MySQL Value

-- Our 'operational monad'

newtype MySQLM a = MySQLM (ReaderT (Text -> IO (), MySQLConn) IO a)
  deriving newtype (Functor,
                    Applicative,
                    Monad,
                    MonadIO,
                    MonadMask,
                    MonadCatch,
                    MonadThrow)

instance MonadFail MySQLM where
  fail err = fail ("Internal error with: " <> err)

newtype NotEnoughColumns = NotEnoughColumns Int
  deriving stock (Show)

instance MonadBeam MySQL MySQLM where
  runReturningMany sql callback = do
    let statement = intoQuery sql
    let debugText = intoDebugText sql
    bracket (acquire statement debugText)
            drainStream
            (\(cols, stream) -> do
              stream' <-
                liftIO .
                  mapM (fromSingleRow . V.zipWith (\col -> (columnType col,)) cols) $
                  stream
              callback (liftIO . read $ stream'))
    where
      acquire s dt = MySQLM . ReaderT $ \(dbg, conn) -> do
        dbg dt
        queryVector_ conn s
      drainStream (_, stream) = liftIO . skipToEof $ stream
  runReturningList sql = MySQLM . ReaderT $ go
    where
      go (dbg, conn) = do
        let statement = intoQuery sql
        bracket (do
                  dbg . intoDebugText $ sql
                  queryVector_ conn statement)
                (\(_, stream) -> skipToEof stream)
                (\(cols, stream) -> do
                  stream' <-
                    mapM (fromSingleRow . V.zipWith (\col -> (columnType col,)) cols) stream
                  toList stream')
  runReturningOne :: forall a . (FromBackendRow MySQL a) => MysqlSyntax -> MySQLM (Maybe a)
  runReturningOne sql = MySQLM . ReaderT $ go
    where
      go (dbg, conn) = do
        let statement = intoQuery sql
        bracket (do
                  dbg . intoDebugText $ sql
                  queryVector_ conn statement)
                (\(_, stream) -> skipToEof stream)
                processOneResult
      processOneResult (cols, stream) = do
        mRes <- read stream
        case mRes of
          Nothing -> pure Nothing
          Just res -> do
            don'tCare <- peek stream
            case don'tCare of
              Nothing ->
                Just <$> (fromSingleRow . V.zipWith (\col -> (columnType col,)) cols $ res)
              Just _  -> pure Nothing
  runNoReturn sql = MySQLM . ReaderT $ go
    where
      go (dbg, conn) = do
        let statement = intoQuery sql
        dbg . intoDebugText $ sql
        _ <- liftIO . execute_ conn $ statement
        pure ()

instance Exception NotEnoughColumns where
  displayException (NotEnoughColumns colCount) =
    "Not enough columns when reading MySQL row. Only have " <>
    show colCount <>
    " column(s)."

data CouldNotReadColumn = CouldNotReadColumn {
  errColIndex   :: {-# UNPACK #-} !Int,
  errColMessage :: !String
  }
  deriving stock (Show)

instance Exception CouldNotReadColumn where
  displayException e =
    "Could not read column " <>
    (show . errColIndex $ e) <>
    ": " <>
    errColMessage e

runBeamMySQL :: MySQLConn -> MySQLM a -> IO a
runBeamMySQL conn (MySQLM comp) = runReaderT comp (\_ -> pure (), conn)

runBeamMySQLDebug :: (Text -> IO ()) -> MySQLConn -> MySQLM a -> IO a
runBeamMySQLDebug dbg conn (MySQLM comp) = runReaderT comp (dbg, conn)

-- Helpers

fromSingleRow :: forall a . (FromBackendRow MySQL a) => Vector (FieldType, MySQLValue) -> IO a
fromSingleRow v = case runExcept (evalStateT (iterM go churched) v) of
  Left err  -> throwIO err
  Right val -> pure val
  where
    FromBackendRowM churched :: FromBackendRowM MySQL a = fromBackendRow
    go :: forall b .
      FromBackendRowF MySQL (StateT (Vector (FieldType, MySQLValue)) (Except BeamRowReadError) b) ->
      StateT (Vector (FieldType, MySQLValue)) (Except BeamRowReadError) b
    go = \case
      ParseOneField callback -> do
        current <- gets (fromField . V.head)
        case current of
          Left err  -> throwError . BeamRowReadError Nothing $ err
          Right val -> do
            modify V.tail
            callback val
      Alt (FromBackendRowM opt1) (FromBackendRowM opt2) callback -> do
        captured <- get
        catchError (callback =<< iterM go opt1)
                   (\_ -> put captured >> (callback =<< iterM go opt2))
      FailParseWith err -> throwError err
