{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}

module Database.Beam.MySQL.Syntax.Render where

import           Data.HashSet (HashSet)
import           Data.Kind (Type)
import           Data.Text (Text)
import           Data.Vector (Vector)
import           Database.MySQL.Base (Param, Query)

newtype RenderError = NotLatin1 Text
  deriving newtype (Eq)
  deriving stock (Show)

data RenderResult = RenderResult
  {
    query      :: !Query,
    parameters :: {-# UNPACK #-} !(Vector Param),
    tables     :: !(HashSet Text)
  }

class RenderInto (e :: Type) (r :: Type) (a :: Type) | r -> e, a -> r where
  renderPass :: a -> Either e r
