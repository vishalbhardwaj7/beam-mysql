{-# LANGUAGE CPP #-}

module Database.Beam.MySQL
(
  MySQL, MySQLM,
  runInsertRowReturning,
  ColumnDecodeError (..), MySQLStatementError (..),
  runBeamMySQL, runBeamMySQLDebug,
  dumpInsertSQL, dumpSelectSQL, dumpUpdateSQL, dumpDeleteSQL,
  ViaJson(..), FakeUTC(..),
  MySQLValueSyntax,
  ParsingMethod(..),
  parsingMethod
) where

import           Data.FakeUTC (FakeUTC (FakeUTC))
import           Data.ViaJson (ViaJson (ViaJson))
import           Database.Beam.MySQL.Connection
import           Database.Beam.MySQL.Extra
import           Database.Beam.MySQL.Syntax.Value

-- | Describes how the library parses fields.
--
-- @since 1.2.1.1
data ParsingMethod = Lenient | Strict
  deriving stock (Eq, Show)

-- | Specifies what parsing method this instance of the library was compiled
-- with. The @lenient@ flag determines what this is set to.
--
-- /See also:/ @beam-mysql.cabal@, @LENIENT.md@
--
-- @since 1.2.1.1
parsingMethod :: ParsingMethod
#ifdef LENIENT
parsingMethod = Lenient
#else
parsingMethod = Strict
#endif
