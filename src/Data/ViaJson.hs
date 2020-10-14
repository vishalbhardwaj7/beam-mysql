module Data.ViaJson where

import           Data.Aeson (FromJSON, ToJSON)
import           Data.Hashable (Hashable)
import           Data.Kind (Type)
import           Type.Reflection (Typeable)

newtype ViaJson (a :: Type) = ViaJson a
  deriving newtype (Eq, Ord, FromJSON, ToJSON, Hashable)
  deriving stock (Show, Typeable)
