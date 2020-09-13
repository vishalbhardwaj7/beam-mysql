{-# LANGUAGE KindSignatures #-}

module Database.Beam.MySQL.Syntax.Render where

import           Data.HashSet (HashSet)
import           Data.Kind (Type)
import           Data.Text (Text)
import           Data.Vector (Vector)
import           Database.Beam.MySQL.Syntax.Delete (MySQLDelete)
import           Database.Beam.MySQL.Syntax.Insert (MySQLInsert)
import           Database.Beam.MySQL.Syntax.Select (MySQLSelect)
import           Database.Beam.MySQL.Syntax.Update (MySQLUpdate)
import           Database.MySQL.Base (Param, Query)
import           Fmt (Builder)

data RenderError =
  NotLatin1 {-# UNPACK #-} !Text |
  CharLengthTooLarge {-# UNPACK #-} !Word |
  VarCharLengthTooLarge {-# UNPACK #-} !Word |
  InvalidBitLength {-# UNPACK #-} !Word |
  InvalidVarBitLength {-# UNPACK #-} !Word
  deriving stock (Show, Eq)

data RenderResult = RenderResult
  {
    queryFragment :: {-# UNPACK #-} !Builder,
    parameters    :: {-# UNPACK #-} !(Vector Param),
    tables        :: !(HashSet Text)
  }

renderSelect :: MySQLSelect -> Either RenderError (HashSet Text, Query)
renderSelect = _

renderInsert :: MySQLInsert -> Either RenderError (HashSet Text, Query)
renderInsert = _

renderUpdate :: MySQLInsert -> Either RenderError (HashSet Text, Query)
renderUpdate = _

renderDelete :: MySQLDelete -> Either RenderError (HashSet Text, Query)
renderDelete = _
