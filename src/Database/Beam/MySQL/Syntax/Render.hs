{-# LANGUAGE KindSignatures #-}

module Database.Beam.MySQL.Syntax.Render where

import           Data.ByteString.Lazy (ByteString)
import           Data.HashSet (HashSet)
import           Data.Kind (Type)
import           Data.Text (Text)
import           Data.Vector (Vector)
import           Database.MySQL.Base (Param)

data RenderError =
  NotLatin1 {-# UNPACK #-} !Text |
  CharLengthTooLarge {-# UNPACK #-} !Word |
  VarCharLengthTooLarge {-# UNPACK #-} !Word |
  InvalidBitLength {-# UNPACK #-} !Word |
  InvalidVarBitLength {-# UNPACK #-} !Word
  deriving stock (Show, Eq)

data RenderResult = RenderResult
  {
    queryFragment :: !ByteString,
    parameters    :: {-# UNPACK #-} !(Vector Param),
    tables        :: !(HashSet Text)
  }

class Renderable (a :: Type) where
  renderPass :: a -> Either RenderError RenderResult

contextFreeRender :: (a -> ByteString) -> a -> Either RenderError RenderResult
contextFreeRender f x = Right . RenderResult (f x) mempty $ mempty
