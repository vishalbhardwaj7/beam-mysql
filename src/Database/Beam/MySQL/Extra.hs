{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE MonoLocalBinds      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Database.Beam.MySQL.Extra where

import           Control.Exception.Safe (bracket)
import           Control.Monad (void)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (ask)
import           Data.Foldable (fold)
import           Data.Functor.Identity (Identity)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.Kind (Type)
import           Data.List (intersperse)
import           Data.Text (Text)
import           Data.Text.Lazy (fromStrict, toStrict)
import           Data.Text.Lazy.Encoding (encodeUtf8)
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Database.Beam (FromBackendRow, runSelectReturningOne)
import           Database.Beam.Backend.SQL (insertCmd, updateCmd)
import           Database.Beam.MySQL.Connection (MySQL, MySQLM (MySQLM), MySQLMEnv (DebugEnv, ReleaseEnv),
                                                 acquireStream, drainStream)
import           Database.Beam.MySQL.Syntax (MysqlInsertSyntax (Insert), MysqlInsertValuesSyntax (FromExprs, FromSQL),
                                             MysqlSyntax,
                                             MysqlTableNameSyntax (MysqlTableNameSyntax),
                                             backtickWrap, defaultE,
                                             intoLazyText, intoQuery,
                                             intoTableName, textSyntax)
import           Database.Beam.Query (SqlDelete (SqlDelete),
                                      SqlInsert (SqlInsert, SqlInsertNoRows),
                                      SqlSelect (SqlSelect),
                                      SqlUpdate (SqlIdentityUpdate, SqlUpdate))
import           Database.MySQL.Base (MySQLConn, MySQLValue (MySQLText),
                                      Query (Query), execute_, okAffectedRows)
import           Prelude hiding (map, read)
import           System.IO.Streams (read)
import           System.IO.Streams.Combinators (foldM, map)

dumpInsertSQL :: forall (table :: (Type -> Type) -> Type) .
  SqlInsert MySQL table -> Maybe Text
dumpInsertSQL = \case
  SqlInsertNoRows -> Nothing
  SqlInsert _ ins -> Just . toStrict . intoLazyText . insertCmd $ ins

dumpSelectSQL :: forall (a :: Type) .
  SqlSelect MySQL a -> Text
dumpSelectSQL (SqlSelect sel) = toStrict . intoLazyText $ sel

dumpUpdateSQL :: forall (table :: (Type -> Type) -> Type) .
  SqlUpdate MySQL table -> Maybe Text
dumpUpdateSQL = \case
  SqlIdentityUpdate -> Nothing
  SqlUpdate _ upd -> Just . toStrict . intoLazyText . updateCmd $ upd

dumpDeleteSQL :: forall (table :: (Type -> Type) -> Type) .
  SqlDelete MySQL table -> Text
dumpDeleteSQL (SqlDelete _ del) = toStrict . intoLazyText $ del

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
                -- (liftIO . \(_, stream) -> foldM (go fieldVals) HM.empty stream)
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
        else pkValue
