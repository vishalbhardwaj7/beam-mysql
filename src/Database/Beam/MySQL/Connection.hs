{-# LANGUAGE CPP                 #-}
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
import           Data.Proxy (Proxy (Proxy))
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
import           Database.Beam.MySQL.Utils (toSQLTypeName)
#ifdef LENIENT
import           Database.Beam.MySQL.FromField (FromFieldL (fromFieldL),
                                                LenientDecodeError (..),
                                                StrictDecodeError (..))
#else
import           Database.Beam.MySQL.FromField (FromFieldS (fromFieldS),
                                                LenientDecodeError (..),
                                                StrictDecodeError (..))
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
import           Type.Reflection (TyCon, tyConName)

data MySQL = MySQL

instance BeamSqlBackendIsString MySQL String

instance BeamSqlBackendIsString MySQL Text

#ifdef LENIENT
instance BeamBackend MySQL where
  type BackendFromField MySQL = FromFieldL
#else
instance BeamBackend MySQL where
  type BackendFromField MySQL = FromFieldS
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
  Decode (ExceptT (Either Int LenientDecodeError)
          (RWS (Vector MySQLValue) () Int) a)
  deriving newtype (Functor,
                    Applicative,
                    Monad,
                    MonadError (Either Int LenientDecodeError),
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
      Right err   ->
        let (tyName, ft, ix', v') = disassembleLenient err v lastIx in
          case err of
            SomeStrictError err' -> case err' of
              UnexpectedNull _ -> FoundUnexpectedNull tyName ft tables ix'
              TypeMismatch _ -> Can'tDecodeIntoDemanded tyName ft tables ix' v'
              Won'tFit _ -> ValueWon'tFitIntoType tyName ft tables ix' v'
            IEEENaN _ -> LenientUnexpectedNaN tyName ft tables ix'
            IEEEInfinity _ -> LenientUnexpectedInfinity tyName ft tables ix' v'
            IEEETooSmall _ -> LenientTooSmallToFit tyName ft tables ix' v'
            IEEETooBig _ -> LenientTooBigToFit tyName ft tables ix' v'
            TextCouldNotParse _ -> LenientTextCouldn'tParse tyName ft tables ix' v'

decodeFromRow :: Int -> FromBackendRowF MySQL (Decode a) -> Decode a
decodeFromRow needed = \case
  ParseOneField callback -> do
    curr <- currentValue
    case curr of
      Nothing -> throwError . Left $ needed
      Just val -> case fromFieldL val of
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
  Decode (ExceptT (Either Int StrictDecodeError)
          (RWS (Vector MySQLValue) () Int) a)
  deriving newtype (Functor,
                    Applicative,
                    Monad,
                    MonadError (Either Int StrictDecodeError),
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
      Right err ->
        let (tyName, ft, ix', v') = disassembleStrict err v lastIx in
          case err of
            UnexpectedNull _ -> FoundUnexpectedNull tyName ft tables ix'
            TypeMismatch _ -> Can'tDecodeIntoDemanded tyName ft tables ix' v'
            Won'tFit _ -> ValueWon'tFitIntoType tyName ft tables ix' v'

decodeFromRow :: Int -> FromBackendRowF MySQL (Decode a) -> Decode a
decodeFromRow needed = \case
  ParseOneField callback -> do
    curr <- currentValue
    case curr of
      Nothing -> throwError . Left $ needed
      Just val -> case fromFieldS val of
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

disassembleLenient ::
  LenientDecodeError ->
  Vector (FieldType, MySQLValue) ->
  Int ->
  (Text, Text, Word, Text)
disassembleLenient err v lastIx = case err of
  SomeStrictError err' -> disassembleStrict err' v lastIx
  IEEENaN t -> (tyConNameText t,
                toSQLTypeName . fst $ v V.! lastIx,
                fromIntegral lastIx,
                pack . show . snd $ v V.! lastIx)
  IEEEInfinity t -> (tyConNameText t,
                toSQLTypeName . fst $ v V.! lastIx,
                fromIntegral lastIx,
                pack . show . snd $ v V.! lastIx)
  IEEETooSmall t -> (tyConNameText t,
                toSQLTypeName . fst $ v V.! lastIx,
                fromIntegral lastIx,
                pack . show . snd $ v V.! lastIx)
  IEEETooBig t -> (tyConNameText t,
                toSQLTypeName . fst $ v V.! lastIx,
                fromIntegral lastIx,
                pack . show . snd $ v V.! lastIx)
  TextCouldNotParse t -> (tyConNameText t,
                toSQLTypeName . fst $ v V.! lastIx,
                fromIntegral lastIx,
                pack . show . snd $ v V.! lastIx)


disassembleStrict ::
  StrictDecodeError ->
  Vector (FieldType, MySQLValue) ->
  Int ->
  (Text, Text, Word, Text)
disassembleStrict err v lastIx =
  let ft = toSQLTypeName . fst $ v V.! lastIx
      ix' = fromIntegral lastIx
      v' = pack . show . snd $ v V.! lastIx in
    case err of
      UnexpectedNull t -> (tyConNameText t, ft, ix', v')
      TypeMismatch t   -> (tyConNameText t, ft, ix', v')
      Won'tFit t       -> (tyConNameText t, ft, ix', v')
