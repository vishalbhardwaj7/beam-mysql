{-# LANGUAGE DeriveAnyClass #-}

module DB.PrimaryKey.NoAutoInc (NoAutoIncT(..)) where

import           Data.Int (Int64)
import           Data.Kind (Type)
import           Data.Text (Text)
import           Database.Beam (Beamable, Columnar,
                                Table (PrimaryKey, primaryKey))
import           GHC.Generics (Generic)
import           Prelude hiding (id)

data NoAutoIncT (f :: Type -> Type) = NoAutoIncT
  { id  :: Columnar f Int64,
    dat :: Columnar f Text
  }
  deriving stock (Generic)
  deriving anyclass (Beamable)

instance Table NoAutoIncT where
  data PrimaryKey NoAutoIncT (f :: Type -> Type) =
    NoAutoIncTPK (Columnar f Int64)
    deriving stock (Generic)
    deriving anyclass (Beamable)
  primaryKey = NoAutoIncTPK . id
