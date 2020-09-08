{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RankNTypes     #-}
{-# LANGUAGE TypeFamilies   #-}

module Main (main) where

import           Data.Functor.Identity (Identity)
import           Data.Kind (Type)
import           Data.Text (Text)
import           Database.Beam (Beamable, Columnar, Database, DatabaseSettings,
                                Table (PrimaryKey, primaryKey), TableEntity,
                                defaultDbSettings)
import           Database.Beam.MySQL (MySQL, dumpDeleteSQL, dumpInsertSQL,
                                      dumpSelectSQL, dumpUpdateSQL)
import           Database.Beam.Query (QBaseScope, QExpr, QFieldAssignment,
                                      SqlDelete, SqlInsert, SqlSelect,
                                      SqlUpdate, all_, delete, filter_, insert,
                                      insertValues, select, toNewValue,
                                      updateTable, val_, (==.))
import           GHC.Generics (Generic)
import           Test.Hspec (describe, hspec, it, shouldBe)
import qualified Test.Hspec as Hspec
import qualified Data.Text as T
import           Control.Monad (forM_)

-- See "mysql-haskell".Database.MySQL.Protocol.Escape
-- and  <http://dev.mysql.com/doc/refman/5.7/en/string-literals.html>

-- zeroSeq :: Text
-- zeroSeq = T.cons (TI.safe $ Char.chr 0) ""

-- TODO: change how we handle "\0" if `text` decides to use UTF8 internally
escapeSequences :: [(Text, Text)]
escapeSequences =
  [ -- ("0", "\0")  -- An ASCII NUL (X'00') character -- TODO: fix this
    ("\'", "\'") -- A single quote (“'”) character
  , ("\"", "\"") -- A double quote (“"”) character
  , ("b", "\b")  -- A backspace character
  , ("n", "\n")  -- A newline (linefeed) character
  , ("r", "\r")  -- A carriage return character
  , ("t", "\t")  -- A tab character
  -- , "\Z"           --	ASCII 26 (Control+Z); see note following the table
  , ("\\", "\\") --	A backslash (“\”) character
  -- , "\%"           --	A “%” character; see note following the table
  -- , "\_"           --	A “_” character; see note following the table
  ]

escapeSequenceSpec :: (Text, Text) -> Hspec.SpecWith ()
escapeSequenceSpec (repr, escapeSeq) = do
  describe ("'\\" <> T.unpack repr <> "' in generated SQL") $ do
    it "should be escaped in INSERT VALUES literals" $
      dumpInsertSQL (insertStmt escapeSeq) `shouldBe`
        Just ("INSERT INTO `test_table`(text) VALUES ('foo\\" <> repr <> "')")
    it "should be escaped in SELECT WHERE literals" $
      dumpSelectSQL (selectWhereStmt escapeSeq) `shouldBe`
        "SELECT `t0`.`text` AS `res0` FROM `test_table` AS `t0` WHERE (`t0`.`text`) = ('foo\\" <> repr <> "')"
    it "should be escaped in UPDATE WHERE literals" $
      dumpUpdateSQL (updateWhereStmt escapeSeq) `shouldBe`
        Just ("UPDATE `test_table` SET `text`='bar\\" <> repr <> "' WHERE (`text`) = ('foo\\" <> repr <> "')")
    it "should be escaped in DELETE WHERE literals" $
      dumpDeleteSQL (deleteWhereStmt escapeSeq) `shouldBe`
        ("DELETE FROM `test_table` WHERE (`text`) = ('foo\\" <> repr <> "')")

main :: IO ()
main = hspec $ do
  forM_ escapeSequences $ escapeSequenceSpec

-- Helpers

insertStmt :: Text -> SqlInsert MySQL TestT
insertStmt escapeSeq =
  insert (_testTestTable testDB) . insertValues $ [
    TestT $ "foo" <> escapeSeq
    ]

selectWhereStmt :: Text -> SqlSelect MySQL (TestT Identity)
selectWhereStmt escapeSeq = select . filter_ go . all_ . _testTestTable $ testDB
  where
    go :: TestT (QExpr MySQL QBaseScope) -> QExpr MySQL QBaseScope Bool
    go row = _testText row ==. val_ ("foo" <> escapeSeq)

updateWhereStmt :: Text -> SqlUpdate MySQL TestT
updateWhereStmt escapeSeq = updateTable (_testTestTable testDB) upd wher
  where
    upd :: TestT (QFieldAssignment MySQL TestT)
    upd = TestT (toNewValue (val_ $ "bar" <> escapeSeq))
    wher :: forall s . TestT (QExpr MySQL s) -> QExpr MySQL s Bool
    wher row = _testText row ==. (val_ $ "foo" <> escapeSeq)

deleteWhereStmt :: Text -> SqlDelete MySQL TestT
deleteWhereStmt escapeSeq = delete (_testTestTable testDB) go
  where
    go :: (forall s' . TestT (QExpr MySQL s')) -> QExpr MySQL s Bool
    go row = _testText row ==. (val_ $ "foo" <> escapeSeq)

newtype TestT (f :: Type -> Type) = TestT
  {
    _testText :: Columnar f Text
  }
  deriving stock (Generic)
  deriving anyclass (Beamable)

instance Table TestT where
  data PrimaryKey TestT (f :: Type -> Type) =
    TestTPK (Columnar f Text)
    deriving stock (Generic)
    deriving anyclass (Beamable)
  primaryKey = TestTPK . _testText

newtype TestDB (f :: Type -> Type) = TestDB
  {
    _testTestTable :: f (TableEntity TestT)
  }
  deriving stock (Generic)
  deriving anyclass (Database MySQL)

testDB :: DatabaseSettings MySQL TestDB
testDB = defaultDbSettings
