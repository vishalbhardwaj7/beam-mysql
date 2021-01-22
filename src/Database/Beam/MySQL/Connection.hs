-- Due to RDP plugin. - Koz
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy         #-}
{-# LANGUAGE TypeFamilies        #-}

module Database.Beam.MySQL.Connection where

import           Control.Exception.Safe (Exception, MonadCatch, MonadMask,
                                         MonadThrow, bracket, throw)
import           Control.Monad (void)
import           Control.Monad.Except (ExceptT, MonadError, catchError,
                                       runExceptT, throwError)
import           Control.Monad.Free.Church (F, iterM)
import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.RWS.Strict (RWS, runRWS)
import           Control.Monad.Reader (MonadReader (ask), ReaderT (..), asks,
                                       runReaderT)
import           Control.Monad.State.Strict (MonadState (get, put), modify)
import           Data.Aeson (FromJSON)
import           Data.ByteString (ByteString)
import           Data.FakeUTC (FakeUTC)
import           Data.HashSet (HashSet)
import           Data.Int (Int16, Int32, Int64, Int8)
import           Data.Kind (Type)
import           Data.Proxy (Proxy (Proxy))
import           Data.Scientific (Scientific)
import           Data.Text (Text, pack)
import           Data.Text.Lazy (toStrict)
import           Data.Text.Lazy.Encoding (decodeUtf8)
import           Data.Time (Day, LocalTime, TimeOfDay)
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Data.ViaJson (ViaJson)
import           Data.Word (Word16, Word32, Word64, Word8)
import           Database.Beam.Backend (BeamBackend (..), BeamSqlBackend,
                                        BeamSqlBackendSyntax,
                                        FromBackendRow (..),
                                        FromBackendRowF (..),
                                        FromBackendRowM (..), MonadBeam (..))
import           Database.Beam.Backend.SQL (BeamSqlBackendIsString, SqlNull)
import           Database.Beam.MySQL.Syntax.Render (RenderError (..),
                                                    RenderErrorType (..),
                                                    renderDelete, renderInsert,
                                                    renderSelect, renderUpdate)
import           Database.Beam.MySQL.Utils (toSQLTypeName)
#ifdef LENIENT
import           Database.Beam.MySQL.FromField (DecodeError (DecodeError),
                                                FromFieldLenient (fromFieldLenient),
                                                Lenient (..), Strict (..))
#else
import           Database.Beam.MySQL.FromField (DecodeError (DecodeError),
                                                FromFieldStrict (fromFieldStrict),
                                                Strict (..))
#endif
import           Database.Beam.MySQL.Syntax (MySQLSyntax (..))
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
import           Type.Reflection (TyCon, Typeable, tyConName)

-- | Backend type for MySQL and MariaDB.
--
-- @since 1.2.2.0
data MySQL = MySQL

-- | @since 1.2.2.0
instance BeamSqlBackendIsString MySQL String

-- | @since 1.2.2.0
instance BeamSqlBackendIsString MySQL Text

#ifdef LENIENT
-- | @since 1.2.2.0
instance BeamBackend MySQL where
  type BackendFromField MySQL = FromFieldLenient
#else
-- | @since 1.2.2.0
instance BeamBackend MySQL where
    type BackendFromField MySQL = FromFieldStrict
#endif

-- | @since 1.2.2.0
instance BeamSqlBackend MySQL

-- | @since 1.2.2.0
type instance BeamSqlBackendSyntax MySQL = MySQLSyntax

-- | @since 1.2.2.0
instance HasQBuilder MySQL where
  buildSqlQuery = buildSql92Query' True

-- Instances for all types we can read using this library from the database

-- | @since 1.2.2.0
instance FromBackendRow MySQL Bool

-- | @since 1.2.2.0
instance HasSqlEqualityCheck MySQL Bool

-- | @since 1.2.2.0
instance HasSqlQuantifiedEqualityCheck MySQL Bool

-- | @since 1.2.2.0
instance FromBackendRow MySQL Int8

-- | @since 1.2.2.0
instance HasSqlEqualityCheck MySQL Int8

-- | @since 1.2.2.0
instance HasSqlQuantifiedEqualityCheck MySQL Int8

-- | @since 1.2.2.0
instance FromBackendRow MySQL Int16

-- | @since 1.2.2.0
instance HasSqlEqualityCheck MySQL Int16

-- | @since 1.2.2.0
instance HasSqlQuantifiedEqualityCheck MySQL Int16

-- | @since 1.2.2.0
instance FromBackendRow MySQL Int32

-- | @since 1.2.2.0
instance HasSqlEqualityCheck MySQL Int32

-- | @since 1.2.2.0
instance HasSqlQuantifiedEqualityCheck MySQL Int32

-- | @since 1.2.2.0
instance FromBackendRow MySQL Int64

-- | @since 1.2.2.0
instance HasSqlEqualityCheck MySQL Int64

-- | @since 1.2.2.0
instance HasSqlQuantifiedEqualityCheck MySQL Int64

-- | @since 1.2.2.0
instance FromBackendRow MySQL Int

-- | @since 1.2.2.0
instance HasSqlEqualityCheck MySQL Int

-- | @since 1.2.2.0
instance HasSqlQuantifiedEqualityCheck MySQL Int

-- | @since 1.2.2.0
instance FromBackendRow MySQL Word8

-- | @since 1.2.2.0
instance HasSqlEqualityCheck MySQL Word8

-- | @since 1.2.2.0
instance HasSqlQuantifiedEqualityCheck MySQL Word8

-- | @since 1.2.2.0
instance FromBackendRow MySQL Word16

-- | @since 1.2.2.0
instance HasSqlEqualityCheck MySQL Word16

-- | @since 1.2.2.0
instance HasSqlQuantifiedEqualityCheck MySQL Word16

-- | @since 1.2.2.0
instance FromBackendRow MySQL Word32

-- | @since 1.2.2.0
instance HasSqlEqualityCheck MySQL Word32

-- | @since 1.2.2.0
instance HasSqlQuantifiedEqualityCheck MySQL Word32

-- | @since 1.2.2.0
instance FromBackendRow MySQL Word64

-- | @since 1.2.2.0
instance HasSqlEqualityCheck MySQL Word64

-- | @since 1.2.2.0
instance HasSqlQuantifiedEqualityCheck MySQL Word64

-- | @since 1.2.2.0
instance FromBackendRow MySQL Word

-- | @since 1.2.2.0
instance HasSqlEqualityCheck MySQL Word

-- | @since 1.2.2.0
instance HasSqlQuantifiedEqualityCheck MySQL Word

-- | @since 1.2.2.0
instance FromBackendRow MySQL Float

-- | @since 1.2.2.0
instance HasSqlEqualityCheck MySQL Float

-- | @since 1.2.2.0
instance HasSqlQuantifiedEqualityCheck MySQL Float

-- | @since 1.2.2.0
instance FromBackendRow MySQL Double

-- | @since 1.2.2.0
instance HasSqlEqualityCheck MySQL Double

-- | @since 1.2.2.0
instance HasSqlQuantifiedEqualityCheck MySQL Double

-- | @since 1.2.2.0
instance FromBackendRow MySQL Scientific

-- | @since 1.2.2.0
instance HasSqlEqualityCheck MySQL Scientific

-- | @since 1.2.2.0
instance HasSqlQuantifiedEqualityCheck MySQL Scientific

-- | @since 1.2.2.0
instance FromBackendRow MySQL SqlNull

-- | @since 1.2.2.0
instance HasSqlEqualityCheck MySQL SqlNull

-- | @since 1.2.2.0
instance HasSqlQuantifiedEqualityCheck MySQL SqlNull

-- | @since 1.2.2.0
instance FromBackendRow MySQL ByteString

-- | @since 1.2.2.0
instance HasSqlEqualityCheck MySQL ByteString

-- | @since 1.2.2.0
instance HasSqlQuantifiedEqualityCheck MySQL ByteString

-- | @since 1.2.2.0
instance FromBackendRow MySQL Text

-- | @since 1.2.2.0
instance HasSqlEqualityCheck MySQL Text

-- | @since 1.2.2.0
instance HasSqlQuantifiedEqualityCheck MySQL Text

-- | @since 1.2.2.0
instance FromBackendRow MySQL LocalTime

-- | @since 1.2.2.0
instance HasSqlEqualityCheck MySQL LocalTime

-- | @since 1.2.2.0
instance HasSqlQuantifiedEqualityCheck MySQL LocalTime

-- | @since 1.2.2.0
instance FromBackendRow MySQL Day

-- | @since 1.2.2.0
instance HasSqlEqualityCheck MySQL Day

-- | @since 1.2.2.0
instance HasSqlQuantifiedEqualityCheck MySQL Day

-- | @since 1.2.2.0
instance FromBackendRow MySQL TimeOfDay

-- | @since 1.2.2.0
instance HasSqlEqualityCheck MySQL TimeOfDay

-- | @since 1.2.2.0
instance HasSqlQuantifiedEqualityCheck MySQL TimeOfDay

-- | @since 1.2.2.0
instance (Typeable a, FromJSON a) => FromBackendRow MySQL (ViaJson a)

-- | @since 1.2.2.0
instance (Typeable a, FromJSON a) => HasSqlEqualityCheck MySQL (ViaJson a)

-- | @since 1.2.2.0
instance (Typeable a, FromJSON a) => HasSqlQuantifiedEqualityCheck MySQL (ViaJson a)

-- | @since 1.2.2.0
instance FromBackendRow MySQL FakeUTC

-- | @since 1.2.2.0
instance HasSqlEqualityCheck MySQL FakeUTC

-- | @since 1.2.2.0
instance HasSqlQuantifiedEqualityCheck MySQL FakeUTC

-- | Some possible (but predictable) failure modes of this backend.
--
-- @since 1.2.2.0
data MySQLStatementError =
  -- | This back-end does not support a given operation in this context.
  --
  -- @since 1.2.2.0
  OperationNotSupported {
    operationName :: {-# UNPACK #-} !Text,
    context       :: {-# UNPACK #-} !Text,
    statement     :: {-# UNPACK #-} !Text
    } |
  -- | This expression would be impure, and therefore cannot be safely replayed.
  -- This can arise when using 'runInsertRowReturning', as we have to simulate
  -- this functionality on older versions of MySQL, requiring us to be able to
  -- replay expressions.
  --
  -- @since 1.2.2.0
  ImpureExpression {
    expression :: {-# UNPACK #-} !Text,
    statement  :: {-# UNPACK #-} !Text
    }
  deriving stock (
                  Eq -- ^ @since 1.2.2.0
                  , Show -- ^ @since 1.2.2.0
                  )

-- | @since 1.2.2.0
instance Exception MySQLStatementError

-- | Represents various forms of failure on column decodes.
--
-- @since 1.2.3.0
data ColumnDecodeError =
  -- | We received a @NULL@ for a column that isn't @NULLABLE@.
  --
  -- @since 1.2.2.0
  FoundUnexpectedNull {
    demandedType   :: {-# UNPACK #-} !Text,
    sqlType        :: {-# UNPACK #-} !Text,
    tablesInvolved :: !(HashSet Text),
    columnIndex    :: {-# UNPACK #-} !Word
    } |
  -- | We cannot decode the given SQL type into the demanded Haskell type.
  --
  -- @since 1.2.2.0
  Can'tDecodeIntoDemanded {
    demandedType   :: {-# UNPACK #-} !Text,
    sqlType        :: {-# UNPACK #-} !Text,
    tablesInvolved :: !(HashSet Text),
    columnIndex    :: {-# UNPACK #-} !Word,
    value          :: {-# UNPACK #-} !Text
    } |
  -- | The SQL value provided by the database does not fit into the
  -- representation of the demanded Haskell type.
  --
  -- @since 1.2.2.0
  ValueWon'tFitIntoType {
    demandedType   :: {-# UNPACK #-} !Text,
    sqlType        :: {-# UNPACK #-} !Text,
    tablesInvolved :: !(HashSet Text),
    columnIndex    :: {-# UNPACK #-} !Word,
    value          :: {-# UNPACK #-} !Text
    } |
  -- | When parsing leniently, we found a NaN. As the demanded type is allowed
  -- to vary considerably, we cannot handle NaNs in general; in the cases where
  -- we can't, this error arises.
  --
  -- /See also:/ @LENIENT.md@
  --
  -- @since 1.2.2.0
  LenientUnexpectedNaN {
    demandedType   :: {-# UNPACK #-} !Text,
    sqlType        :: {-# UNPACK #-} !Text,
    tablesInvolved :: !(HashSet Text),
    columnIndex    :: {-# UNPACK #-} !Word
    } |
  -- | When parsing leniently, we found a negative or positive infinity. As the
  -- demanded type is allowed to vary considerably, we cannot handle this case
  -- in general; in those situations where we can't, this error arises.
  --
  -- /See also:/ @LENIENT.md@
  --
  -- @since 1.2.2.0
  LenientUnexpectedInfinity {
    demandedType   :: {-# UNPACK #-} !Text,
    sqlType        :: {-# UNPACK #-} !Text,
    tablesInvolved :: !(HashSet Text),
    columnIndex    :: {-# UNPACK #-} !Word,
    value          :: {-# UNPACK #-} !Text
    } |
  -- | When parsing leniently, we found a value that's too negative to fit into
  -- the representation requested.
  --
  -- /See also:/ @LENIENT.md@
  --
  -- @since 1.2.2.0
  LenientTooSmallToFit {
    demandedType   :: {-# UNPACK #-} !Text,
    sqlType        :: {-# UNPACK #-} !Text,
    tablesInvolved :: !(HashSet Text),
    columnIndex    :: {-# UNPACK #-} !Word,
    value          :: {-# UNPACK #-} !Text
    } |
  -- | When parsing leniently, we found a value that's too large (that is, too
  -- far along the number line in the positive direction) to fit into the
  -- representation requested.
  --
  -- /See also:/ @LENIENT.md@
  --
  -- @since 1.2.2.0
  LenientTooBigToFit {
    demandedType   :: {-# UNPACK #-} !Text,
    sqlType        :: {-# UNPACK #-} !Text,
    tablesInvolved :: !(HashSet Text),
    columnIndex    :: {-# UNPACK #-} !Word,
    value          :: {-# UNPACK #-} !Text
    } |
  -- | When parsing leniently, we could not decode a value of the requested type
  -- from a textual representation.
  --
  -- /See also:/ @LENIENT.md@
  --
  -- @since 1.2.2.0
  LenientTextCouldn'tParse {
    demandedType   :: {-# UNPACK #-} !Text,
    sqlType        :: {-# UNPACK #-} !Text,
    tablesInvolved :: !(HashSet Text),
    columnIndex    :: {-# UNPACK #-} !Word,
    value          :: {-# UNPACK #-} !Text
    } |
  -- | There were not enough fields returned by the database to satisfy the
  -- requested type.
  --
  -- @since 1.2.2.0
  DemandedTooManyFields {
    fieldsAvailable :: {-# UNPACK #-} !Word,
    fieldsDemanded  :: {-# UNPACK #-} !Word
    } |
  -- | When using the 'ViaJson' newtype wrapper to aid parsing, something went
  -- wrong with parsing the JSON representation of the desired value.
  --
  -- @since 1.2.2.0
  JSONError {
    demandedType   :: {-# UNPACK #-} !Text,
    sqlType        :: {-# UNPACK #-} !Text,
    tablesInvolved :: !(HashSet Text),
    columnIndex    :: {-# UNPACK #-} !Word,
    value          :: {-# UNPACK #-} !Text
    }
  deriving stock (
                  Eq -- ^ @since 1.2.2.0
                  , Show -- ^ @since 1.2.2.0
                  )

-- | @since 1.2.2.0
instance Exception ColumnDecodeError

-- Small helper for our environment
data MySQLMEnv =
  DebugEnv !(Text -> IO ())
           {-# UNPACK #-} !MySQLConn |
  ReleaseEnv {-# UNPACK #-} !MySQLConn

-- | Operational monad for MySQL/MariaDB Beam actions.
--
-- @since 1.2.2.0
newtype MySQLM (a :: Type) = MySQLM (ReaderT MySQLMEnv IO a)
  deriving (Functor -- ^ @since 1.2.2.0
            , Applicative -- ^ @since 1.2.2.0
            , Monad -- ^ @since 1.2.2.0
            , MonadIO -- ^ @since 1.2.2.0
            , MonadMask -- ^ @since 1.2.2.0
            , MonadCatch -- ^ @since 1.2.2.0
            , MonadThrow -- ^ @since 1.2.2.0
            ) via (ReaderT MySQLMEnv IO)

-- | @since 1.2.2.0
instance MonadFail MySQLM where
  fail err = error ("Internal error with: " <> err)

-- | @since 1.2.2.0
instance MonadBeam MySQL MySQLM where
  {-# INLINABLE runNoReturn #-}
  runNoReturn sql = do
    (stmt, conn, _) <- processAndLog sql
    void . liftIO . execute_ conn $ stmt
  {-# INLINABLE runReturningOne #-}
  runReturningOne sql = do
    (stmt, conn, tables) <- processAndLog sql
    bracket (acquireStream conn stmt)
            drainStream
            (processOneResult tables)
  {-# INLINABLE runReturningList #-}
  runReturningList sql = do
    (stmt, conn, tables) <- processAndLog sql
    bracket (acquireStream conn stmt)
            drainStream
            (\stream -> liftIO (S.toList =<< parseRowsIO tables stream))
  {-# INLINABLE runReturningMany #-}
  runReturningMany sql callback = do
    (stmt, conn, tables) <- processAndLog sql
    bracket (acquireStream conn stmt)
            drainStream
            (\stream -> callback $ liftIO (read =<< parseRowsIO tables stream))

-- | Run a series of MySQL operations without debugging.
--
-- @since 1.2.2.0
runBeamMySQL :: MySQLConn -> MySQLM a -> IO a
runBeamMySQL conn (MySQLM comp) = runReaderT comp . ReleaseEnv $ conn

-- | Run a series of MySQL operations with debug output provided by the handler.
--
-- @since 1.2.2.0
runBeamMySQLDebug :: (Text -> IO ()) -> MySQLConn -> MySQLM a -> IO a
runBeamMySQLDebug dbg conn (MySQLM comp) =
  runReaderT comp . DebugEnv dbg $ conn

-- Helpers

-- Rendering aid

renderMySQL :: MySQLSyntax -> Either RenderError (Query, HashSet Text)
renderMySQL = \case
  ASelect sql  -> renderSelect sql
  AnInsert sql -> renderInsert sql
  AnUpdate sql -> renderUpdate sql
  ADelete sql  -> renderDelete sql

-- Removes some duplication from MonadBeam instance

processAndLog :: MySQLSyntax -> MySQLM (Query, MySQLConn, HashSet Text)
processAndLog sql = do
  let rendered = renderMySQL sql
  case rendered of
    Left (RenderError typ ast) -> case typ of
      UnsupportedOperation op ->
        throw (OperationNotSupported op ast (pack . show $ sql))
    Right (stmt@(Query inner), tables) -> do
      env <- MySQLM ask
      conn <- case env of
        DebugEnv dbg conn -> do
          let textual = toStrict . decodeUtf8 $ inner
          liftIO (dbg textual) >> pure conn
        ReleaseEnv conn -> pure conn
      pure (stmt, conn, tables)

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
    Just res ->
      case runDecode (iterM (decodeFromRow needed) churched) res tables of
        Left err -> throw err
        Right x  -> pure . Just $ x
  where
    churched :: F (FromBackendRowF MySQL) a
    FromBackendRowM churched = fromBackendRow
    needed :: Int
    needed = valuesNeeded (Proxy @MySQL) (Proxy @a)

parseRowsIO :: forall (a :: Type) .
  (FromBackendRow MySQL a) =>
  HashSet Text ->
  InputStream (Vector (FieldType, MySQLValue)) ->
  IO (InputStream a)
parseRowsIO tables = mapM go
  where
    go :: Vector (FieldType, MySQLValue) -> IO a
    go v = case runDecode (iterM (decodeFromRow needed) churched) v tables of
      Left err -> throw err
      Right x  -> pure x
    churched :: F (FromBackendRowF MySQL) a
    FromBackendRowM churched = fromBackendRow
    needed :: Int
    needed = valuesNeeded (Proxy @MySQL) (Proxy @a)

-- Decoding from a row is complex enough to warrant its own operators and an
-- explicit stack.

#ifdef LENIENT
newtype Decode (a :: Type) =
  Decode (ExceptT (Either Int (DecodeError Lenient))
          (RWS (Vector MySQLValue) () Int) a)
  deriving newtype (Functor,
                    Applicative,
                    Monad,
                    MonadError (Either Int (DecodeError Lenient)),
                    MonadReader (Vector MySQLValue),
                    MonadState Int)

runDecode :: forall (a :: Type) .
  Decode a ->
  Vector (FieldType, MySQLValue) ->
  HashSet Text ->
  Either ColumnDecodeError a
runDecode (Decode comp) v tables =
  case runRWS (runExceptT comp) (V.map snd v) 0 of
    (Right res, _, _) -> Right res
    (Left failure, lastIx, _) -> Left $ case failure of
      Left needed ->
        DemandedTooManyFields (fromIntegral lastIx + 1) . fromIntegral $ needed
      Right (DecodeError err typ)   ->
        let ft = toSQLTypeName . fst $ v V.! lastIx
            ix' = fromIntegral lastIx
            v' = pack . show . snd $ v V.! lastIx
            tyName = tyConNameText typ in
          case err of
            SomeStrict err' -> case err' of
              UnexpectedNull -> FoundUnexpectedNull tyName ft tables ix'
              TypeMismatch   -> Can'tDecodeIntoDemanded tyName ft tables ix' v'
              Won'tFit       -> ValueWon'tFitIntoType tyName ft tables ix' v'
              NotValidJSON   -> JSONError tyName ft tables ix' v'
            IEEENaN -> LenientUnexpectedNaN tyName ft tables ix'
            IEEEInfinity -> LenientUnexpectedInfinity tyName ft tables ix' v'
            IEEETooSmall -> LenientTooSmallToFit tyName ft tables ix' v'
            IEEETooBig -> LenientTooBigToFit tyName ft tables ix' v'
            TextCouldNotParse -> LenientTextCouldn'tParse tyName ft tables ix' v'

decodeFromRow :: Int -> FromBackendRowF MySQL (Decode a) -> Decode a
decodeFromRow needed = \case
  ParseOneField callback -> do
    curr <- currentValue
    case curr of
      Nothing -> throwError . Left $ needed
      Just val -> case fromFieldLenient val of
        Left err -> throwError . Right $ err
        Right x  -> advanceIndex >> callback x
  Alt (FromBackendRowM opt1) (FromBackendRowM opt2) callback -> do
    ix <- get -- save state
    -- The 'catch-in-catch' here is needed due to the rather peculiar way beam
    -- parses NULLable columns. Essentially, it first tries to grab a non-NULL
    -- value, then, if it fails, tries to unconditionally grab a NULL.
    --
    -- This is encoded as an Alt. Therefore, if we don't want strange false
    -- positives regarding NULL parses, we have to forward the _first_ error we
    -- saw.
    catchError (callback =<< iterM (decodeFromRow needed) opt1)
               (\err -> do
                  put ix -- restore our state to how it was
                  catchError (callback =<< iterM (decodeFromRow needed) opt2)
                             (\_ -> throwError err))
  FailParseWith err -> error ("Leaked beam internal with: " <> show err)
#else
newtype Decode (a :: Type) =
  Decode (ExceptT (Either Int (DecodeError Strict))
          (RWS (Vector MySQLValue) () Int) a)
  deriving newtype (Functor,
                    Applicative,
                    Monad,
                    MonadError (Either Int (DecodeError Strict)),
                    MonadReader (Vector MySQLValue),
                    MonadState Int)

runDecode :: forall (a :: Type) .
  Decode a ->
  Vector (FieldType, MySQLValue) ->
  HashSet Text ->
  Either ColumnDecodeError a
runDecode (Decode comp) v tables =
  case runRWS (runExceptT comp) (V.map snd v) 0 of
    (Right res, _, _) -> Right res
    (Left failure, lastIx, _) -> Left $ case failure of
      Left needed ->
        DemandedTooManyFields (fromIntegral lastIx + 1) . fromIntegral $ needed
      Right (DecodeError err typ) ->
        let ft = toSQLTypeName . fst $ v V.! lastIx
            ix' = fromIntegral lastIx
            v' = pack . show . snd $ v V.! lastIx
            tyName = tyConNameText typ in
          case err of
            UnexpectedNull -> FoundUnexpectedNull tyName ft tables ix'
            TypeMismatch   -> Can'tDecodeIntoDemanded tyName ft tables ix' v'
            Won'tFit       -> ValueWon'tFitIntoType tyName ft tables ix' v'
            NotValidJSON   -> JSONError tyName ft tables ix' v'

decodeFromRow :: Int -> FromBackendRowF MySQL (Decode a) -> Decode a
decodeFromRow needed = \case
  ParseOneField callback -> do
    curr <- currentValue
    case curr of
      Nothing -> throwError . Left $ needed
      Just val -> case fromFieldStrict val of
        Left err -> throwError . Right $ err
        Right x  -> advanceIndex >> callback x
  Alt (FromBackendRowM opt1) (FromBackendRowM opt2) callback -> do
    ix <- get -- save state
    -- The 'catch-in-catch' here is needed due to the rather peculiar way beam
    -- parses NULLable columns. Essentially, it first tries to grab a non-NULL
    -- value, then, if it fails, tries to unconditionally grab a NULL.
    --
    -- This is encoded as an Alt. Therefore, if we don't want strange false
    -- positives regarding NULL parses, we have to forward the _first_ error we
    -- saw.
    catchError (callback =<< iterM (decodeFromRow needed) opt1)
               (\err -> do
                  put ix -- restore our state to how it was
                  catchError (callback =<< iterM (decodeFromRow needed) opt2)
                             (\_ -> throwError err))
  FailParseWith err -> error ("Leaked beam internal with: " <> show err)
#endif

currentValue :: Decode (Maybe MySQLValue)
currentValue = do
  ix <- get
  asks (V.!? ix)

advanceIndex :: Decode ()
advanceIndex = modify succ

tyConNameText :: TyCon -> Text
tyConNameText = pack . tyConName
