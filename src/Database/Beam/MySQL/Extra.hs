{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE MonoLocalBinds      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Database.Beam.MySQL.Extra where

import           Control.Exception.Safe (bracket, throw)
import           Control.Monad (void)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (asks)
import           Data.Foldable (traverse_)
import           Data.Functor.Identity (Identity)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.Kind (Type)
import           Data.Text (Text, pack)
import           Data.Text.Lazy (fromStrict, toStrict)
import           Data.Text.Lazy.Encoding (decodeUtf8, encodeUtf8)
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Database.Beam.Backend.SQL (FromBackendRow, insertCmd,
                                            runNoReturn)
import           Database.Beam.MySQL.Connection (MySQL, MySQLM (..),
                                                 MySQLMEnv (..),
                                                 MySQLStatementError (..),
                                                 acquireStream, drainStream)
import           Database.Beam.MySQL.Syntax (MySQLSyntax (AnInsert))
import           Database.Beam.MySQL.Syntax.Insert (MySQLInsert (InsertStmt),
                                                    MySQLInsertValuesSyntax (..))
import           Database.Beam.MySQL.Syntax.Render (RenderError (..),
                                                    RenderErrorType (..),
                                                    renderDelete, renderInsert,
                                                    renderSelect, renderUpdate)
import           Database.Beam.MySQL.Syntax.Select (MySQLExpressionSyntax (Default),
                                                    MySQLSelect (..),
                                                    MySQLSelectTableSyntax (..),
                                                    MySQLTableNameSyntax (..),
                                                    Purity (..),
                                                    TableRowExpression (..))
import           Database.Beam.Query (SqlDelete (SqlDelete),
                                      SqlInsert (SqlInsert, SqlInsertNoRows),
                                      SqlSelect (SqlSelect),
                                      SqlUpdate (SqlIdentityUpdate, SqlUpdate),
                                      runInsert, runSelectReturningOne)
import           Database.MySQL.Base (FieldType, MySQLConn,
                                      MySQLValue (MySQLText), Query (Query),
                                      execute_, okAffectedRows)
import           Prelude hiding (map, read)
import           System.IO.Streams (InputStream, read)
import           System.IO.Streams.Combinators (foldM, map)

dumpInsertSQL :: forall (table :: (Type -> Type) -> Type) .
  SqlInsert MySQL table -> Maybe Text
dumpInsertSQL = \case
  SqlInsertNoRows -> Nothing
  SqlInsert _ ins -> case renderInsert ins of
    Left _                 -> Nothing
    Right (_, Query query) -> Just . toStrict . decodeUtf8 $ query

dumpSelectSQL :: forall (a :: Type) .
  SqlSelect MySQL a -> Maybe Text
dumpSelectSQL (SqlSelect sel) = case renderSelect sel of
  Left _                 -> Nothing
  Right (_, Query query) -> Just . toStrict . decodeUtf8 $ query

dumpUpdateSQL :: forall (table :: (Type -> Type) -> Type) .
  SqlUpdate MySQL table -> Maybe Text
dumpUpdateSQL = \case
  SqlIdentityUpdate -> Nothing
  SqlUpdate _ upd -> case renderUpdate upd of
    Left _                 -> Nothing
    Right (_, Query query) -> Just . toStrict . decodeUtf8 $ query

dumpDeleteSQL :: forall (table :: (Type -> Type) -> Type) .
  SqlDelete MySQL table -> Maybe Text
dumpDeleteSQL (SqlDelete _ del) = case renderDelete del of
  Left _                 -> Nothing
  Right (_, Query query) -> Just . toStrict . decodeUtf8 $ query

runInsertRowReturning :: forall (table :: (Type -> Type) -> Type) .
  (FromBackendRow MySQL (table Identity)) =>
  SqlInsert MySQL table -> MySQLM (Maybe (table Identity))
runInsertRowReturning stmt = case stmt of
  SqlInsertNoRows -> pure Nothing
  SqlInsert _ ins -> case renderInsert ins of
    Left (RenderError typ ast)         -> case typ of
      UnsupportedOperation op ->
        throw (OperationNotSupported op ast (pack . show $ ins))
    Right (_, query) -> case ins.insertValues of
      InsertFromSQL{} -> throw . insertIntoSelect $ ins
      insertExprs -> case V.length insertExprs.rowsToInsert of
        0 -> pure Nothing -- should be impossible
        1 -> do
          let expr = V.head insertExprs.rowsToInsert
          go ins expr query
        _ ->
          throw . multipleRowInserts insertExprs.rowsToInsert $ ins
  where
    go ::
      MySQLInsert ->
      TableRowExpression ->
      Query ->
      MySQLM (Maybe (table Identity))
    go ins (TableRowExpression v) query = do
      -- ensure that we have a pure expression
      traverse_ (analyzeExpr (pack . show $ ins)) v
      -- if we haven't thrown, proceed
      let fieldVals = HM.fromList . V.toList . V.zip ins.columns $ v
      let pkStatement = buildPkQuery ins.tableName.name
      conn <- getConnection
      -- get primary key, and what values would change there
      pkColVals <- getPkColVals conn fieldVals pkStatement
      -- Determine if we have an autoincrement column, and if so, what it is.
      mAIColumn <- collectAutoIncrementColumn conn ins.tableName.name
      -- Run the insert.
      res <- liftIO . execute_ conn $ query
      case okAffectedRows res of
        0 -> pure Nothing -- nothing changed, so nothing to give back
        _ -> case mAIColumn >>= \aiCol -> HM.lookup aiCol pkColVals of
          -- If we have no autoincrement column, or the column isn't part of the
          -- primary key, we can ignore it, and use the primary key directly.
          Nothing -> do
            let sel = SqlSelect . SelectStmt mempty -- this gets ignored anyway
                                             (SelectTableStatement
                                                mempty -- this gets ignored anyway
                                                Nothing -- no quantifier needed
                                                _ -- projection
                                                _ -- FROM
                                                _ -- WHERE
                                                Nothing -- no GROUP BY
                                                Nothing) -- no HAVING
                                             mempty -- order doesn't matter
                                             (Just 1) -- LIMIT 1
                                             $ Nothing -- no offset
            runSelectReturningOne sel
          -- Otherwise, we have to build a fresh select targeting the columns
          -- that changed, and their values, then query on this.
          Just _  -> do
            let sel = SqlSelect . SelectStmt mempty -- this gets ignored anyway
                                             _
                                             mempty -- order doesn't matter
                                             (Just 1) -- LIMIT 1
                                             $ Nothing -- no offset
            runSelectReturningOne sel
          {-
      case mAIColumn of
        -- No autoincrement column, so primary key is enough to select changed
        -- rows.
        Nothing -> insertNoAutoincrement conn pkColVals query
        Just aiCol  -> case HM.lookup aiCol pkColVals of
          -- The autoincrement column isn't part of the primary key, so it
          -- doesn't matter.
          Nothing -> insertNoAutoincrement conn pkColVals query
          Just _ -> do
            liftIO . void . execute_ conn $ query
            -- Build a fresh select targeting the values that changed, then
            -- query on that.
            let newPKSel = _
            runSelectReturningOne newPKSel
            -}
    insertIntoSelect :: MySQLInsert -> MySQLStatementError
    insertIntoSelect ins =
      OperationNotSupported "Insert-into-select with returning"
                            (pack . show $ ins)
                            (pack . show $ ins)
    multipleRowInserts ::
      Vector TableRowExpression -> MySQLInsert -> MySQLStatementError
    multipleRowInserts insertExprs ins =
      OperationNotSupported "Multiple row inserts with returning"
                            (pack . show $ insertExprs)
                            (pack . show $ ins)

-- Helpers

analyzeExpr :: Text -> MySQLExpressionSyntax -> MySQLM ()
analyzeExpr t e = case e.ann.purity of
  Pure -> pure ()
  Impure -> case e of
    Default _ -> pure ()
    _         -> throw . ImpureExpression (pack . show $ e) $ t

buildPkQuery :: Text -> Query
buildPkQuery nam = Query $
  "SELECT key_column_usage.column_name " <>
  "FROM information_schema.key_column_usage " <>
  "WHERE table_schema = schema() " <>
  "AND constraint_name = 'PRIMARY' " <>
  "AND table_name '" <>
  (encodeUtf8 . fromStrict $ nam) <>
  "';"

getConnection :: MySQLM MySQLConn
getConnection = MySQLM (asks go)
  where
    go :: MySQLMEnv -> MySQLConn
    go = \case
      DebugEnv _ c -> c
      ReleaseEnv c -> c

getPkColVals ::
  MySQLConn ->
  HashMap Text MySQLExpressionSyntax ->
  Query ->
  MySQLM (HashMap Text MySQLExpressionSyntax)
getPkColVals conn fieldVals query =
  bracket (acquireStream conn query)
          drainStream
          (liftIO . foldM go HM.empty)
  where
    go ::
      HashMap Text MySQLExpressionSyntax ->
      Vector (FieldType, MySQLValue) ->
      IO (HashMap Text MySQLExpressionSyntax)
    go acc v = case v V.!? 0 of
      Just (_, MySQLText t) -> case HM.lookup t fieldVals of
        Nothing  -> pure acc
        Just val -> pure . HM.insert t val $ acc
      -- This outcome is literally impossible. If this happens, it's enough of a
      -- bug that we _should_ crash.
      _ -> error "Column name was not text."

collectAutoIncrementColumn :: MySQLConn -> Text -> MySQLM (Maybe Text)
collectAutoIncrementColumn conn nam = do
  let aiQuery = Query $
        "SELECT column_name " <>
        "FROM information_schema.columns " <>
        "WHERE table_schema = schema() " <>
        "AND table_name = '" <>
        (encodeUtf8 . fromStrict $ nam) <>
        "' AND extra LIKE 'auto_increment';"
  bracket (acquireStream conn aiQuery)
          drainStream
          (liftIO . go)
  where
    go ::
      InputStream (Vector (FieldType, MySQLValue)) ->
      IO (Maybe Text)
    go stream = do
      res <- read stream
      case (V.!? 0) =<< res of
        Just (_, MySQLText aic) -> pure . Just $ aic
        _                       -> pure Nothing

    {-
  where
    go ::
      MySQLInsert ->
      MySQLTableNameSyntax ->
      Vector Text ->
      TableRowExpression ->
      MySQLM (Maybe (table Identity))
    go ins tableName' fields (TableRowExpression v) = do
      -- ensure that we have a pure expression
      traverse_ (analyzeExpr textified) v
      -- if we haven't thrown, we can proceed
      let fieldVals = HM.fromList . V.toList . V.zip fields $ v
      -- get primary key, and what values would change there
      let pkStatement = buildPkQuery tableName'
      conn <- callDownConnection
      pkColVals <- getPkColVals conn fieldVals pkStatement
      mAIColumn <- collectAutoIncrementColumn conn tableName'
      case mAIColumn of
        -- No autoincrement column, so primary key is enough to select changed
        -- rows.
        Nothing -> insertNoAutoincrement conn pkColVals ins
        Just aiCol -> case HM.lookup aiCol pkColVals of
          -- The autoincrement column isn't part of the primary key, so it
          -- doesn't matter.
          Nothing -> insertNoAutoincrement conn pkColVals ins
          Just _  -> do
            runInsert stmt
            -- This is a gory hack.
            let newPKs = getNewPKs aiCol pkColVals
            selectByPKCols newPKs
    textified :: Text
    textified = pack . show . dumpInsertSQL $ stmt

-- Helpers

analyzeExpr :: Text -> MySQLExpressionSyntax -> MySQLM ()
analyzeExpr textified expr = case expr.ann.purity of
  Pure   -> pure ()
  Impure -> case expr of
    Default _ -> pure ()
    _         -> throw . ImpureExpression (pack . show $ expr) $ textified

buildPkQuery :: MySQLTableNameSyntax -> Query
buildPkQuery (TableName _ nam) = Query $
  "SELECT key_column_usage.column_name " <>
  "FROM information_schema.key_column_usage " <>
  "WHERE table_schema = schema() " <>
  "AND constraint_name = 'PRIMARY' " <>
  "AND table_name '" <>
  (encodeUtf8 . fromStrict $ nam) <>
  "';"

callDownConnection :: MySQLM MySQLConn
callDownConnection = do
  env <- MySQLM ask
  case env of
    DebugEnv _ c -> pure c
    ReleaseEnv c -> pure c

getPkColVals :: forall (a :: Type) .
  MySQLConn -> HashMap Text a -> Query -> MySQLM (HashMap Text a)
getPkColVals conn fieldVals query =
  bracket (acquireStream conn query)
          drainStream
          go
  where
    go ::
      InputStream (Vector (FieldType, MySQLValue)) ->
      MySQLM (HashMap Text a)
    go stream = liftIO $ do
      stream' <- map (V.map snd) stream
      foldM go2 HM.empty stream'
    go2 :: HashMap Text a -> Vector MySQLValue -> IO (HashMap Text a)
    go2 acc v = case v V.!? 0 of
      Just (MySQLText t) -> case HM.lookup t fieldVals of
        Nothing  -> pure acc
        Just val -> pure . HM.insert t val $ acc
      -- This outcome is literally impossible. If it happens, it's enough of a
      -- bug that we _should_ crash.
      _                  -> error "Column name was not text."

collectAutoIncrementColumn ::
  MySQLConn -> MySQLTableNameSyntax -> MySQLM (Maybe Text)
collectAutoIncrementColumn conn tableName' = do
  let aiQuery = Query $
        "SELECT column_name " <>
        "FROM information_schema.columns " <>
        "WHERE table_schema = schema() " <>
        "AND table_anem = '" <>
        (encodeUtf8 . fromStrict $ tableName'.name) <>
        "' AND extra LIKE 'auto_increment';"
  bracket (acquireStream conn aiQuery)
          drainStream
          go
  where
    go ::
      InputStream (Vector (FieldType, MySQLValue)) ->
      MySQLM (Maybe Text)
    go stream = liftIO $ do
      res <- read stream
      case (V.!? 0) =<< res of
        Just (_, MySQLText aic) -> pure . Just $ aic
        _                       -> pure Nothing

insertNoAutoincrement ::
  MySQLConn ->
  HashMap Text MySQLExpressionSyntax ->
  MySQLInsert ->
  MySQLM (Maybe (table Identity))
insertNoAutoincrement conn pkColVals ins = do
  let insertStatement = _ ins
  res <- liftIO . execute_ conn $ insertStatement
  case okAffectedRows res of
    0 -> pure Nothing -- query had no effect, so nothing to give
    _ -> selectByPKCols pkColVals

getNewPKs :: Text -> HashMap Text MySQLExpressionSyntax -> _
getNewPKs = _

selectByPKCols :: _ -> MySQLM (Maybe (table Identity))
selectByPKCols = _
-}

{-
runInsertRowReturning :: forall (table :: (Type -> Type) -> Type) .
  (FromBackendRow MySQL (table Identity)) =>
  SqlInsert MySQL table -> MySQLM (Maybe (table Identity))
runInsertRowReturning = \case
  SqlInsertNoRows -> pure Nothing
  SqlInsert _ ins@(Insert tableName fields values) -> case values of
    FromSQL _ -> fail "Not implemented for INSERT INTO ... SELECT ..."
    FromExprs [] -> pure Nothing -- should be impossible
    FromExprs [expr] -> do
      let fieldVals = HM.fromList . zip fields $ expr
      -- get primary key and what values would change there
      let pkStatement = buildPkQuery tableName
      conn <- (\case DebugEnv _ c -> c
                     ReleaseEnv c -> c) <$>MySQLM ask
      pkColVals <-
        bracket (acquireStream conn pkStatement)
                drainStream
                (\stream -> liftIO (map (V.map snd) stream >>=
                              foldM (go fieldVals) HM.empty))
      let MysqlTableNameSyntax _ nameOfTable = tableName
      mAutoincCol <- collectAutoIncrementCol conn nameOfTable
      case mAutoincCol of
        -- No autoincrementing column, so primary key is enough to select
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
                (\stream -> liftIO $ do
                    res <- read stream
                    case (V.!? 0) =<< res of
                      Just (_, MySQLText aic) -> pure . Just $ aic
                      _                       -> pure Nothing)
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
        else pkValue -}
