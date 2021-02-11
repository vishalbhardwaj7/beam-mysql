{-# LANGUAGE DeriveAnyClass #-}

module DB.PrimaryKey.None (NoneT(..)) where

import           Data.Int (Int64)
import           Data.Kind (Type)
import           Data.Text (Text)
import           Database.Beam (Beamable, Columnar,
                                Table (PrimaryKey, primaryKey))
import           GHC.Generics (Generic)
import           Prelude hiding (id)

data NoneT (f :: Type -> Type) = NoneT
  { id  :: Columnar f Int64,
    dat :: Columnar f Text
  }
  deriving stock (Generic)
  deriving anyclass (Beamable)

instance Table NoneT where
  data PrimaryKey NoneT (f :: Type -> Type) =
    NoneTPK (Columnar f Int64)
    deriving stock (Generic)
    deriving anyclass (Beamable)
  primaryKey = NoneTPK . id
