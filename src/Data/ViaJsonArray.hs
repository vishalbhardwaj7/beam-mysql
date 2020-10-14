module Data.ViaJsonArray where

import           Data.Text (Text)
import           Data.Vector (Vector)

newtype ViaJsonArray = ViaJsonArray (Vector Text)
  deriving newtype (Eq, Ord)
  deriving stock (Show)
