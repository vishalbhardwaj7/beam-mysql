{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Database.Beam.MySQL.Connection where

import           Control.Exception.Safe (Exception, MonadCatch, MonadMask,
                                         MonadThrow, bracket, throw)
import           Control.Monad (void)
import           Control.Monad.Except (ExceptT, MonadError, catchError,
                                       runExceptT, throwError)
import           Control.Monad.Free.Church (F, iterM)
import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Reader (MonadReader (ask), ReaderT (..), asks,
                                       runReaderT)
import           Control.Monad.RWS.Strict (RWS, runRWS)
import           Control.Monad.State.Strict (MonadState (get, put), modify)
import           Data.ByteString (ByteString)
import           Data.HashSet (HashSet)
import           Data.Int (Int16, Int32, Int64, Int8)
import           Data.Kind (Type)
import           Data.Scientific (Scientific)
import           Data.Text (Text, pack)
import           Data.Time (Day, LocalTime, NominalDiffTime, TimeOfDay)
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Data.Word (Word16, Word32, Word64, Word8)
import           Database.Beam.Backend (BeamBackend (..), BeamSqlBackend,
                                        BeamSqlBackendSyntax,
                                        FromBackendRow (..),
                                        FromBackendRowF (..),
                                        FromBackendRowM (..), MonadBeam (..))
import           Database.Beam.Backend.SQL (BeamSqlBackendIsString, SqlNull)
import qualified Database.Beam.MySQL.FromField as FromField
import           Database.Beam.MySQL.Utils (toSQLTypeName)
#ifdef LENIENT
import           Database.Beam.MySQL.FromField (FromField (..),
                                                Leniency (Lenient))
#else
import           Database.Beam.MySQL.FromField (FromField (..),
                                                Leniency (Strict))
#endif
import           Database.Beam.MySQL.Syntax (MysqlSyntax (..), intoDebugText,
                                             intoQuery)
import           Database.Beam.Query (HasQBuilder (..), HasSqlEqualityCheck,
                                      HasSqlQuantifiedEqualityCheck)
import           Database.Beam.Query.SQL92 (buildSql92Query')
import           Database.MySQL.Base (ColumnDef, FieldType, MySQLConn,
                                      MySQLValue (..), Query (..), columnType,
                                      execute_, queryVector_, skipToEof)
import           Prelude hiding (map, mapM, read)
import           System.IO.Streams (InputStream, read)
import           System.IO.Streams.Combinators (map, mapM)
import qualified System.IO.Streams.List as S

data MySQL = MySQL

instance BeamSqlBackendIsString MySQL String

instance BeamSqlBackendIsString MySQL Text

#ifdef LENIENT
instance BeamBackend MySQL where
  type BackendFromField MySQL = FromField 'Lenient
#else
instance BeamBackend MySQL where
  type BackendFromField MySQL = FromField 'Strict
#endif

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

data ColumnDecodeError =
  FoundUnexpectedNull {
    demandedType   :: {-# UNPACK #-} !Text,
    sqlType        :: {-# UNPACK #-} !Text,
    tablesInvolved :: !(HashSet Text),
    columnIndex    :: {-# UNPACK #-} !Word
    } |
  Can'tDecodeIntoDemanded {
    demandedType   :: {-# UNPACK #-} !Text,
    sqlType        :: {-# UNPACK #-} !Text,
    tablesInvolved :: !(HashSet Text),
    columnIndex    :: {-# UNPACK #-} !Word,
    value          :: {-# UNPACK #-} !Text
    } |
  ValueWon'tFitIntoType {
    demandedType   :: {-# UNPACK #-} !Text,
    sqlType        :: {-# UNPACK #-} !Text,
    tablesInvolved :: !(HashSet Text),
    columnIndex    :: {-# UNPACK #-} !Word,
    value          :: {-# UNPACK #-} !Text
    } |
  LenientUnexpectedNaN {
    demandedType   :: {-# UNPACK #-} !Text,
    sqlType        :: {-# UNPACK #-} !Text,
    tablesInvolved :: !(HashSet Text),
    columnIndex    :: {-# UNPACK #-} !Word
    } |
  LenientUnexpectedInfinity {
    demandedType   :: {-# UNPACK #-} !Text,
    sqlType        :: {-# UNPACK #-} !Text,
    tablesInvolved :: !(HashSet Text),
    columnIndex    :: {-# UNPACK #-} !Word,
    value          :: {-# UNPACK #-} !Text
    } |
  LenientTooSmallToFit {
    demandedType   :: {-# UNPACK #-} !Text,
    sqlType        :: {-# UNPACK #-} !Text,
    tablesInvolved :: !(HashSet Text),
    columnIndex    :: {-# UNPACK #-} !Word,
    value          :: {-# UNPACK #-} !Text
    } |
  LenientTooBigToFit {
    demandedType   :: {-# UNPACK #-} !Text,
    sqlType        :: {-# UNPACK #-} !Text,
    tablesInvolved :: !(HashSet Text),
    columnIndex    :: {-# UNPACK #-} !Word,
    value          :: {-# UNPACK #-} !Text
    } |
  LenientTextCouldn'tParse {
    demandedType   :: {-# UNPACK #-} !Text,
    sqlType        :: {-# UNPACK #-} !Text,
    tablesInvolved :: !(HashSet Text),
    columnIndex    :: {-# UNPACK #-} !Word,
    value          :: {-# UNPACK #-} !Text
    } |
  DemandedTooManyFields {
    fieldsAvailable :: {-# UNPACK #-} !Word,
    fieldsDemanded  :: {-# UNPACK #-} !Word
    }
  deriving stock (Eq, Show)

instance Exception ColumnDecodeError

-- Small helper for our environment
data MySQLMEnv =
  DebugEnv !(Text -> IO ())
           {-# UNPACK #-} !MySQLConn |
  ReleaseEnv {-# UNPACK #-} !MySQLConn

-- Operational monad
newtype MySQLM (a :: Type) = MySQLM (ReaderT MySQLMEnv IO a)
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
  {-# INLINABLE runNoReturn #-}
  runNoReturn sql@(MysqlSyntax (_, _)) = do
    (statement, conn) <- processAndLog sql
    -- TODO: Error capture. - Koz
    void . liftIO . execute_ conn $ statement
  {-# INLINABLE runReturningOne #-}
  runReturningOne sql@(MysqlSyntax (tables, _)) = do
    (statement, conn) <- processAndLog sql
    bracket (acquireStream conn statement)
            drainStream
            (processOneResult tables)
  {-# INLINABLE runReturningList #-}
  runReturningList sql@(MysqlSyntax (tables, _)) = do
    (statement, conn) <- processAndLog sql
    bracket (acquireStream conn statement)
            drainStream
            (\stream -> liftIO (S.toList =<< parseRowsIO tables stream))
  {-# INLINABLE runReturningMany #-}
  runReturningMany sql@(MysqlSyntax (tables, _)) callback = do
    (statement, conn) <- processAndLog sql
    bracket (acquireStream conn statement)
            drainStream
            (\stream -> callback $ liftIO (read =<< parseRowsIO tables stream))

-- Run without debugging
runBeamMySQL :: MySQLConn -> MySQLM a -> IO a
runBeamMySQL conn (MySQLM comp) = runReaderT comp . ReleaseEnv $ conn

-- Run with debugging
runBeamMySQLDebug :: (Text -> IO ()) -> MySQLConn -> MySQLM a -> IO a
runBeamMySQLDebug dbg conn (MySQLM comp) =
  runReaderT comp . DebugEnv dbg $ conn

-- TODO: runInsertRowReturning. - Koz

-- Helpers

-- Removes some duplication from MonadBeam instance

processAndLog :: MysqlSyntax -> MySQLM (Query, MySQLConn)
processAndLog sql = do
  let statement = intoQuery sql
  env <- MySQLM ask
  conn <- case env of
    DebugEnv dbg conn -> do
      liftIO . dbg . intoDebugText $ sql
      pure conn
    ReleaseEnv conn   -> pure conn
  pure (statement, conn)

acquireStream :: (MonadIO m) =>
  MySQLConn -> Query -> m (InputStream (Vector (FieldType, MySQLValue)))
acquireStream conn q = liftIO (queryVector_ conn q >>= go)
  where
    go :: (Vector ColumnDef, InputStream (Vector MySQLValue)) ->
          IO (InputStream (Vector (FieldType, MySQLValue)))
    go (v, stream) = map (V.zip (V.map columnType v)) stream

drainStream :: (MonadIO m) => InputStream a -> m ()
drainStream = liftIO . skipToEof

processOneResult :: forall (a :: Type) .
  (FromBackendRow MySQL a) =>
  HashSet Text -> InputStream (Vector (FieldType, MySQLValue)) -> MySQLM (Maybe a)
processOneResult tables stream = do
  mRes <- liftIO . read $ stream
  case mRes of
    Nothing  -> pure Nothing
    Just res -> case runDecode (iterM decodeFromRow churched) res of
      Left (err, ix) -> throw . toDecodeError tables res ix $ err
      Right x        -> pure . Just $ x
  where
    churched :: F (FromBackendRowF MySQL) a
    FromBackendRowM churched = fromBackendRow

parseRowsIO :: forall (a :: Type) .
  (FromBackendRow MySQL a) =>
  HashSet Text ->
  InputStream (Vector (FieldType, MySQLValue)) ->
  IO (InputStream a)
parseRowsIO tables = mapM go
  where
    go :: Vector (FieldType, MySQLValue) -> IO a
    go v = case runDecode (iterM decodeFromRow churched) v of
      Left (err, ix) -> throw . toDecodeError tables v ix $ err
      Right x        -> pure x
    churched :: F (FromBackendRowF MySQL) a
    FromBackendRowM churched = fromBackendRow

toDecodeError ::
  HashSet Text ->
  Vector (FieldType, MySQLValue) ->
  Int ->
  DecodeError ->
  ColumnDecodeError
toDecodeError tables v ix = \case
  UnexpectedNull t ->
    FoundUnexpectedNull t ft tables (fromIntegral ix)
  TypeMismatch t ->
    Can'tDecodeIntoDemanded t ft tables (fromIntegral ix) valRep
  Won'tFit t ->
    ValueWon'tFitIntoType t ft tables (fromIntegral ix) valRep
  IEEENaN t ->
    LenientUnexpectedNaN t ft tables (fromIntegral ix)
  IEEEInfinity t ->
    LenientUnexpectedInfinity t ft tables (fromIntegral ix) valRep
  IEEETooSmall t ->
    LenientTooSmallToFit t ft tables (fromIntegral ix) valRep
  IEEETooBig t ->
    LenientTooBigToFit t ft tables (fromIntegral ix) valRep
  TextCouldNotParse t ->
    LenientTextCouldn'tParse t ft tables (fromIntegral ix) valRep
  RowExhausted ->
    DemandedTooManyFields (fromIntegral . V.length $ v) (fromIntegral ix + 1)
  BeamInternal -> error "Leak from beam internals!"
  where
    ft :: Text
    ft = toSQLTypeName . fst $ v V.! ix
    valRep :: Text
    valRep = pack . show . snd $ v V.! ix

-- Decoding from a row is complex enough to warrant its own operators and an
-- explicit stack.

data DecodeError =
  UnexpectedNull {-# UNPACK #-} !Text |
  TypeMismatch {-# UNPACK #-} !Text |
  Won'tFit {-# UNPACK #-} !Text |
  IEEENaN {-# UNPACK #-} !Text |
  IEEEInfinity {-# UNPACK #-} !Text |
  IEEETooSmall {-# UNPACK #-} !Text |
  IEEETooBig {-# UNPACK #-} !Text |
  TextCouldNotParse {-# UNPACK #-} !Text |
  RowExhausted |
  BeamInternal -- This shouldn't ever appear.
  deriving stock (Eq, Show)

newtype Decode (a :: Type) =
  Decode (ExceptT DecodeError (RWS (Vector (FieldType, MySQLValue)) () Int) a)
  deriving newtype (Functor,
                    Applicative,
                    Monad,
                    MonadError DecodeError,
                    MonadReader (Vector (FieldType, MySQLValue)),
                    MonadState Int)

runDecode :: forall (a :: Type) .
  Decode a -> Vector (FieldType, MySQLValue) -> Either (DecodeError, Int) a
runDecode (Decode comp) v = go . runExceptT $ comp
  where
    go ::
      RWS (Vector (FieldType, MySQLValue)) () Int (Either DecodeError a) ->
      Either (DecodeError, Int) a
    go comp' = case runRWS comp' v 0 of
      (Left err, ix, _) -> Left (err, ix)
      (Right res, _, _) -> Right res

currentValue :: Decode (Maybe (FieldType, MySQLValue))
currentValue = do
  ix <- get
  asks (V.!? ix)

advanceIndex :: Decode ()
advanceIndex = modify succ

decodeFromRow :: FromBackendRowF MySQL (Decode a) -> Decode a
decodeFromRow = \case
  ParseOneField callback -> do
    curr <- currentValue
    case curr of
      Nothing        -> throwError RowExhausted
      -- TODO: Seems like field types are unnecessary. - Koz
      Just (_, val) -> case fromField val of
        FromField.UnexpectedNull t -> throwError . UnexpectedNull $ t
        FromField.TypeMismatch t   -> throwError . TypeMismatch $ t
        FromField.Won'tFit t       -> throwError . Won'tFit $ t
        FromField.StrictParse x    -> advanceIndex >> callback x
        {-
         - TODO: Write a version of this for lenient parsing. - Koz
        FromField.IEEENaN           -> throwError IEEENaN
        FromField.IEEEInfinity      -> throwError IEEEInfinity
        FromField.IEEETooSmall      -> throwError IEEETooSmall
        FromField.IEEETooBig        -> throwError IEEETooBig
        FromField.TextCouldNotParse -> throwError TextCouldNotParse
        FromField.LenientParse x    -> advanceIndex >> callback x
        -}
  Alt (FromBackendRowM opt1) (FromBackendRowM opt2) callback -> do
    ix <- get
    -- The 'catch-in-catch' here is needed due to the rather peculiar way beam
    -- parses NULLable columns. Essentially, it first tries to grab a non-NULL
    -- value, then, if it fails, tries to unconditionally grab a NULL.
    --
    -- This is encoded as an Alt. Therefore, if we don't want strange false
    -- positives regarding NULL parses, we have to forward the _first_ error we
    -- saw.
    catchError (callback =<< iterM decodeFromRow opt1)
               (\err -> do
                  put ix -- restore our state to how it was
                  catchError (callback =<< iterM decodeFromRow opt2)
                             (\_ -> throwError err))
  FailParseWith _ -> throwError BeamInternal

{-
-- A more useful error context for beam decoding errors
data ColumnDecodeError = ColumnDecodeError {
  tableNames :: !(HashSet Text),
  errorType  :: {-# UNPACK #-} !BeamRowReadError
  }
  deriving stock (Eq, Show)

instance Exception ColumnDecodeError

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
  runNoReturn sql@(MysqlSyntax (tables, _)) = do
    (statement, conn) <- processAndLog sql
    catch (void . liftIO. execute_ conn $ statement)
          (rethrowBeamRowError tables)
  runReturningOne sql@(MysqlSyntax (tables, _)) = do
    (statement, conn) <- processAndLog sql
    bracket (acquireStream conn statement)
            drainStream
            (handle (rethrowBeamRowError tables) . liftIO . processOneResult)
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
  runReturningList sql@(MysqlSyntax (tables, _)) = do
    (statement, conn) <- processAndLog sql
    bracket (acquireStream conn statement)
            drainStream
            (\(fts, stream) ->
              handle (rethrowBeamRowError tables) .
                      liftIO $
                      (S.toList =<< mapM (decodeFromRow fts) stream))
  runReturningMany sql@(MysqlSyntax (tables, _)) callback = do
    (statement, conn) <- processAndLog sql
    bracket (acquireStream conn statement)
            drainStream
            (\(fts, stream) ->
              handle (rethrowBeamRowError tables) .
                callback $
                liftIO (read =<< mapM (decodeFromRow fts) stream))

-- Run without debugging
runBeamMySQL :: MySQLConn -> MySQLM a -> IO a
runBeamMySQL conn (MySQLM comp) = runReaderT comp (\_ -> pure (), conn)

-- Run with debugging
runBeamMySQLDebug :: (Text -> IO ()) -> MySQLConn -> MySQLM a -> IO a
runBeamMySQLDebug dbg conn (MySQLM comp) = runReaderT comp (dbg, conn)

runInsertRowReturning :: forall (table :: (Type -> Type) -> Type) .
  (FromBackendRow MySQL (table Identity)) =>
  SqlInsert MySQL table -> MySQLM (Maybe (table Identity))
runInsertRowReturning = \case
  SqlInsertNoRows -> pure Nothing
  SqlInsert _ ins@(Insert tableName fields values) -> case values of
    FromSQL _ -> fail "Not implemented for INSERT INTO ... SELECT ..."
    FromExprs [] -> pure Nothing -- should be impossible
    FromExprs [expr] -> handle (rethrowBeamRowError (tblNameSet tableName)) $ do
      let fieldVals = HM.fromList . zip fields $ expr
      -- get primary key and what values would change there
      let pkStatement = buildPkQuery tableName
      (_, conn) <- MySQLM ask
      pkColVals <-
        bracket (acquireStream conn pkStatement)
                drainStream
                (liftIO . \(_, stream) -> foldM (go fieldVals) HM.empty stream)
      let MysqlTableNameSyntax _ nameOfTable = tableName
      -- This assumes _one_ auto-increment column. What if there are several?
      -- TODO: Determine proper semantics for this. - Koz
      mAutoincCol <- collectAutoIncrementCol conn nameOfTable
      case mAutoincCol of
        -- No autoincrementing column(s), so primary key is enough to select
        -- changed rows.
        Nothing         -> insertReturningWithoutAutoinc conn pkColVals
        Just autoincCol -> case HM.lookup autoincCol pkColVals of
          -- The autoincrementing column isn't part of the primary key, so it
          -- doesn't matter.
          Nothing -> insertReturningWithoutAutoinc conn pkColVals
          Just _  -> do
            let insertStatement = insertCmd ins
            void . liftIO . execute_ conn . intoQuery $ insertStatement
            -- This is a gory hack.
            -- TODO: Is there a better (or indeed, _any_ other) way to figure
            -- out if an autoincrementing field is set to some value, or
            -- DEFAULT, for example?
            let newPKs = HM.mapWithKey (regraft autoincCol) pkColVals
            selectByPrimaryKeyCols newPKs
    _ -> fail "Cannot insert several rows with runInsertRowReturning"
    where
      buildPkQuery :: MysqlTableNameSyntax -> Query
      buildPkQuery (MysqlTableNameSyntax _ name) = Query (
        "SELECT key_column_usage.column_name " <>
        "FROM information_schema.key_column_usage " <>
        "WHERE table_schema = schema() " <>
        "AND constraint_name = 'PRIMARY' " <>
        "AND table_name = '" <>
        (encodeUtf8 . fromStrict $ name) <>
        "';")
      go :: HashMap Text v -> HashMap Text v -> Vector MySQLValue -> IO (HashMap Text v)
      go fieldVals acc v = do
        colName <- extractColName v
        case HM.lookup colName fieldVals of
          Nothing  -> pure acc
          Just val -> pure . HM.insert colName val $ acc
      extractColName :: Vector MySQLValue -> IO Text
      extractColName v = case V.head v of
        MySQLText t -> pure t
        _           -> fail "Column name was not text"
      -- Select inserted rows by primary keys
      -- Result can be totally wrong if the values are not constant
      -- expressions.
      --
      -- TODO: Tagging of non-constants to block their evaluation. - Koz
      selectByPrimaryKeyCols ::
        HashMap Text MysqlSyntax -> MySQLM (Maybe (table Identity))
      selectByPrimaryKeyCols pkColVals = do
        let queryStatement = SqlSelect . buildPkMatchQuery $ pkColVals
        runSelectReturningOne queryStatement
      fieldsExpr :: MysqlSyntax
      fieldsExpr =
        fold . intersperse ", " . fmap (backtickWrap . textSyntax) $ fields
      buildPkMatchQuery :: HashMap Text MysqlSyntax -> MysqlSyntax
      buildPkMatchQuery pkColVals =
        "SELECT " <>
        fieldsExpr <>
        " FROM " <>
        intoTableName tableName <>
        " WHERE " <>
        (fold . intersperse " AND" . fmap fromPair . HM.toList $ pkColVals) <>
        ";"
      fromPair :: (Text, MysqlSyntax) -> MysqlSyntax
      fromPair (colName, val) = textSyntax colName <> " = " <> val
      collectAutoIncrementCol :: MySQLConn -> Text -> MySQLM (Maybe Text)
      collectAutoIncrementCol conn nameOfTable = do
        let autoIncQuery = Query (
              "SELECT `column_name` " <>
              "FROM `information_schema`.`columns` " <>
              "WHERE `table_schema` = schema() " <>
              "AND `table_name` = '" <>
              (encodeUtf8 . fromStrict $ nameOfTable) <>
              "' AND `extra` LIKE 'auto_increment';"
              )
        bracket (acquireStream conn autoIncQuery)
                drainStream
                (liftIO . \(_, stream) -> do
                    res <- read stream
                    case (V.!? 0) =<< res of
                      Just (MySQLText autoincCol) -> pure . Just $ autoincCol
                      _                           -> pure Nothing)
      insertReturningWithoutAutoinc ::
        MySQLConn ->
        HashMap Text MysqlSyntax ->
        MySQLM (Maybe (table Identity))
      insertReturningWithoutAutoinc conn pkColVals = do
        let insertStatement = insertCmd ins
        res <- liftIO . execute_ conn . intoQuery $ insertStatement
        case okAffectedRows res of
          0 -> pure Nothing
          _ -> selectByPrimaryKeyCols pkColVals
      regraft :: Text -> Text -> MysqlSyntax -> MysqlSyntax
      regraft autoincCol pkName pkValue =
        if pkName == autoincCol && pkValue == defaultE
        then "last_insert_id()"
        else pkValue
      tblNameSet :: MysqlTableNameSyntax -> HashSet Text
      tblNameSet (MysqlTableNameSyntax _ nameOfTable) =
        HS.singleton nameOfTable

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
  Decode (ReaderT (Vector FieldType)
          (ReaderT (Vector MySQLValue)
          (StateT Int (Except BeamRowReadError))) a)
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
        -- The 'catch-in-catch' here is needed due to the rather peculiar way
        -- beam parses NULLable columns. Essentially, it first tries to grab a
        -- value, then, if it fails, it tries to grab a NULL.
        --
        -- This is encoded as an Alt. Therefore, if we don't want strange false
        -- positives regarding NULL parses, we have to forward the _first_ error
        -- we saw.
        --
        -- Ideally, we'd detect this situation properly, but we are stuck with
        -- beam's 'BeamRowReadError' type, which doesn't really provision this.
        catchError (callback =<< iterM go opt1)
                   (\err -> do
                      restoreState captured
                      catchError (callback =<< iterM go opt2)
                                 (\_ -> throwError err))
      FailParseWith err -> throwError err

-- This allows us to report errors in row decodes with additional context
-- (namely, tables involved).
rethrowBeamRowError :: HashSet Text -> BeamRowReadError -> MySQLM a
rethrowBeamRowError tables = throw . ColumnDecodeError tables

-}
