{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Database.Beam.MySQL.Connection (
  MySQL(..),
  MySQLM(..),
  runBeamMySQL, runBeamMySQLDebug, runInsertRowReturning
  ) where

import           Control.Exception.Safe (MonadCatch, MonadMask, MonadThrow,
                                         bracket, throwIO)
import           Control.Monad (void, join)
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
import           Data.Foldable (fold)
import           Data.Functor.Identity (Identity)
import           Data.HashMap.Strict as HM
import           Data.Int (Int16, Int32, Int64, Int8)
import           Data.List (intersperse)
import           Data.Scientific (Scientific)
import           Data.Text (Text)
import           Data.Text.Lazy (fromStrict)
import           Data.Text.Lazy.Builder (fromLazyText, fromText, toLazyText)
import           Data.Text.Lazy.Encoding (encodeUtf8)
import           Data.Time (Day, LocalTime, NominalDiffTime, TimeOfDay)
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Data.Word (Word16, Word32, Word64, Word8)
import           Database.Beam.Backend (BeamBackend (..), BeamRowReadError (..),
                                        BeamSqlBackend, BeamSqlBackendSyntax,
                                        FromBackendRow (..),
                                        FromBackendRowF (..),
                                        FromBackendRowM (..), MonadBeam (..),
                                        insertCmd)
import           Database.Beam.Backend.SQL (BeamSqlBackendIsString, SqlNull)
import           Database.Beam.MySQL.FromField (FromField (..))
import           Database.Beam.MySQL.Syntax (MysqlInsertSyntax (..),
                                             MysqlInsertValuesSyntax (..),
                                             MysqlSyntax (..),
                                             MysqlTableNameSyntax (..),
                                             intoDebugText, intoQuery,
                                             intoTableName, defaultE, backtickWrap,
                                             intoLazyText)
import           Database.Beam.Query (HasQBuilder (..), HasSqlEqualityCheck,
                                      HasSqlQuantifiedEqualityCheck,
                                      SqlInsert (..), SqlSelect (..),
                                      runSelectReturningOne)
import           Database.Beam.Query.SQL92 (buildSql92Query')
import           Database.MySQL.Base (FieldType, MySQLConn, MySQLValue (..),
                                      Query (..), columnType, execute_,
                                      okAffectedRows, queryVector_, skipToEof)
import           Prelude hiding (mapM, read)
import           System.IO.Streams (InputStream, peek, read)
import           System.IO.Streams.Combinators (foldM, mapM)
import qualified System.IO.Streams.List as S

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
  fail err = error ("Internal error with: " <> err)

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
            (\(fts, stream) -> liftIO (S.toList =<< mapM (decodeFromRow fts) stream))
  runReturningMany sql callback = do
    (statement, conn) <- processAndLog sql
    bracket (acquireStream conn statement)
            drainStream
            (\(fts, stream) -> callback (liftIO (read =<< mapM (decodeFromRow fts) stream)))

-- Run without debugging
runBeamMySQL :: MySQLConn -> MySQLM a -> IO a
runBeamMySQL conn (MySQLM comp) = runReaderT comp (\_ -> pure (), conn)

-- Run with debugging
runBeamMySQLDebug :: (Text -> IO ()) -> MySQLConn -> MySQLM a -> IO a
runBeamMySQLDebug dbg conn (MySQLM comp) = runReaderT comp (dbg, conn)

-- TODO: combine old and Koz's description
--
-- TODO: hangs on `fail` -- what's going on?
runInsertRowReturning :: (FromBackendRow MySQL (table Identity)) =>
  SqlInsert MySQL table -> MySQLM (Maybe (table Identity))
runInsertRowReturning = \case
  SqlInsertNoRows -> pure Nothing
  (SqlInsert _ ins@(Insert tableName fields values)) -> case values of
    FromSQL _ -> fail "Not implemented for INSERT INTO ... SELECT ..."
    FromExprs exprs -> case exprs of
      []     -> pure Nothing -- should be impossible
      [expr] -> do
        let fieldVals =
              HM.fromList . zipWith (\t (MysqlSyntax b) -> (t, toLazyText b)) fields $ expr
        -- get primary key and what values would change there
        let pkStatement = buildPkQuery tableName
        (_, conn) <- MySQLM ask
        pkColVals <-
          bracket (acquireStream conn pkStatement)
                  drainStream
                  (liftIO . \(_, stream) -> foldM (go fieldVals) HM.empty stream)

        let selectByPrimaryKeyCols pkColVals' = do
              -- Select inserted rows by Primary Keys
              -- Result can be totally wrong if some of (vals :: MysqlExpressionSyntax) can result in
              -- different values when evaluated by db.
              let queryStatement = SqlSelect . buildPkMatchQuery $ pkColVals'
              runSelectReturningOne queryStatement

        let insertReturningWithoutAutoincrement = do
              let insertStatement = insertCmd ins
              res <- liftIO . execute_ conn . intoQuery $ insertStatement
              case okAffectedRows res of
                0 -> pure Nothing
                _ -> selectByPrimaryKeyCols pkColVals

        let MysqlTableNameSyntax _ nameOfTable = tableName
        (mautoIncrementCol :: Maybe Text) <- do
          let queryAI = Query (
                  "SELECT `column_name` FROM `information_schema`.`columns` WHERE " <>
                  "`table_schema`= schema() AND `table_name`='" <> (encodeUtf8 . fromStrict $ nameOfTable) <>
                  "' AND `extra` LIKE 'auto_increment'"
                )
          bracket (acquireStream conn queryAI)
                  drainStream
                  (liftIO . \(_, stream) -> do
                      res <- read stream
                      case join $ fmap (V.!? 0) res of
                        Just (MySQLText aiCol) -> pure $ Just aiCol
                        _ -> pure Nothing)

        case mautoIncrementCol of
          Nothing -> insertReturningWithoutAutoincrement -- no AI we can use PK to select inserted rows.
          Just aiCol ->
            if not $ HM.member aiCol pkColVals
            then insertReturningWithoutAutoincrement    -- AI exists and not part of PK, so we don't care about it
            else do                                     -- AI exists and is part of PK
              let insertStatement = insertCmd ins
              void $ liftIO . execute_ conn . intoQuery $ insertStatement

              -- hacky. But is there any other way to figure out if AI field is set to some value, or DEFAULT, for example?
              let newPKs = HM.mapWithKey (\pkF pkV ->
                                          if pkF == aiCol && (pkV == intoLazyText defaultE)
                                          then "last_insert_id()"
                                          else pkV) pkColVals
              selectByPrimaryKeyCols newPKs

      _      -> fail "Cannot insert several rows with runInsertRowReturning"
    where
      fieldsExpr = fold . intersperse ", " . fmap (backtickWrap . fromText) $ fields

      go fieldVals acc v = do
        colName <- extractColName v
        case HM.lookup colName fieldVals of
          Nothing  -> pure acc
          Just val -> pure (HM.insert colName val acc)
      extractColName v = case V.head v of
        MySQLText t -> pure t
        _           -> fail "Column name was not text"
      buildPkQuery (MysqlTableNameSyntax _ name) = Query (
        "SELECT key_column_usage.column_name " <>
        "FROM information_schema.key_column_usage " <>
        "WHERE table_schema = schema() " <>
        "AND constraint_name = 'PRIMARY' " <>
        "AND table_name = '" <> (encodeUtf8 . fromStrict $ name) <> "';")
      buildPkMatchQuery pkColVals  =
        "SELECT " <> fieldsExpr <>
        " FROM " <> intoTableName tableName <>
        " WHERE " <> (fold . intersperse " AND " . fmap toPair . HM.toList $ pkColVals) <> ";"
      toPair (colName, val) = MysqlSyntax (fromText colName <> " = " <> fromLazyText val)

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
