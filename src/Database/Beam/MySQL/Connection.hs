{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Database.Beam.MySQL.Connection (
  MySQL(..),
  MySQLM(..),
  runBeamMySQL, runBeamMySQLDebug
  ) where

import           Control.Exception.Safe (MonadCatch, MonadMask, MonadThrow,
                                         bracket, throwIO)
import           Control.Monad (void)
import           Control.Monad.Except (Except, MonadError, catchError,
                                       runExcept, throwError)
import           Control.Monad.Free.Church (iterM)
import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Reader (ReaderT (..), ask, asks, runReaderT)
import           Control.Monad.State.Strict (StateT, evalStateT, get, modify,
                                             put)
import           Control.Monad.Trans (lift)
import           Data.Aeson (Value)
import           Data.Bifunctor (first)
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
import           Database.MySQL.Base (FieldType, MySQLConn, MySQLValue, Query,
                                      columnType, execute_, queryVector_,
                                      skipToEof)
import           Prelude hiding (mapM, read)
import           System.IO.Streams (InputStream, peek, read)
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

instance MonadBeam MySQL MySQLM where
  runNoReturn sql = do
    (statement, conn) <- processAndLog sql
    void . liftIO. execute_ conn $ statement
  runReturningOne sql = do
    (statement, conn) <- processAndLog sql
    bracket (acquireStream conn statement)
            drainStream
            (liftIO . processOneResult)
    where
      processOneResult (fts, stream) = do
        mRes <- read stream
        case mRes of
          Nothing -> pure Nothing
          Just res -> do
            don'tCare <- peek stream
            case don'tCare of
              Nothing -> Just <$> decodeFromRow fts res
              Just _  -> pure Nothing
  runReturningList sql = do
    (statement, conn) <- processAndLog sql
    bracket (acquireStream conn statement)
            drainStream
            (\(fts, stream) -> liftIO (toList =<< mapM (decodeFromRow fts) stream))
  runReturningMany sql callback = do
    (statement, conn) <- processAndLog sql
    bracket (acquireStream conn statement)
            drainStream
            (\(fts, stream) -> callback (liftIO (read =<< mapM (decodeFromRow fts) stream)))

runBeamMySQL :: MySQLConn -> MySQLM a -> IO a
runBeamMySQL conn (MySQLM comp) = runReaderT comp (\_ -> pure (), conn)

runBeamMySQLDebug :: (Text -> IO ()) -> MySQLConn -> MySQLM a -> IO a
runBeamMySQLDebug dbg conn (MySQLM comp) = runReaderT comp (dbg, conn)

-- Helpers

-- Removes some duplication from MonadBeam instance

processAndLog :: MysqlSyntax -> MySQLM (Query, MySQLConn)
processAndLog sql = do
  let statement = intoQuery sql
  (dbg, conn) <- MySQLM ask
  liftIO . dbg . intoDebugText $ sql
  pure (statement, conn)

acquireStream :: (MonadIO m) =>
  MySQLConn -> Query -> m (Vector FieldType, InputStream (Vector MySQLValue))
acquireStream conn = fmap (first (V.map columnType)) . liftIO . queryVector_ conn

drainStream :: (MonadIO m) => (a, InputStream b) -> m ()
drainStream (_, stream) = liftIO . skipToEof $ stream

-- Decoding from a row is complex enough to warrant its own operators and an
-- explicit stack.
newtype Decode a =
  Decode (ReaderT (Vector FieldType) (ReaderT (Vector MySQLValue) (StateT Int (Except BeamRowReadError))) a)
  deriving newtype (Functor, Applicative, Monad, MonadError BeamRowReadError)

runDecode :: Decode a -> Vector FieldType -> Vector MySQLValue -> Either BeamRowReadError a
runDecode (Decode comp) fieldTypes values =
  runExcept (evalStateT (runReaderT (runReaderT comp fieldTypes) values) 0)

captureState :: Decode Int
captureState = Decode . lift . lift $ get

restoreState :: Int -> Decode ()
restoreState s = Decode . lift . lift $ put s

currentColumn :: Decode (Int, FieldType, MySQLValue)
currentColumn = do
  ix <- captureState
  ft <- Decode . asks $ (V.! ix)
  val <- Decode . lift . asks $ (V.! ix)
  pure (ix, ft, val)

advanceColumn :: Decode ()
advanceColumn = Decode . lift . lift $ modify (+ 1)

decodeFromRow :: forall a . (FromBackendRow MySQL a) => Vector FieldType -> Vector MySQLValue -> IO a
decodeFromRow fieldTypes values = case runDecode (iterM go churched) fieldTypes values of
  Left err  -> throwIO err
  Right val -> pure val
  where
    FromBackendRowM churched :: FromBackendRowM MySQL a = fromBackendRow
    go :: forall b . FromBackendRowF MySQL (Decode b) -> Decode b
    go = \case
      ParseOneField callback -> do
        (ix, ft, vals) <- currentColumn
        case fromField ft vals of
          Left err -> throwError . BeamRowReadError (Just ix) $ err
          Right v  -> advanceColumn >> callback v
      Alt (FromBackendRowM opt1) (FromBackendRowM opt2) callback -> do
        captured <- captureState
        catchError (callback =<< iterM go opt1)
                   (\_ -> restoreState captured >> (callback =<< iterM go opt2))
      FailParseWith err -> throwError err
