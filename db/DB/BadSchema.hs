{-# LANGUAGE DeriveAnyClass #-}

module DB.BadSchema (BadSchemaT(..)) where

import           Data.Int (Int64)
import           Data.Kind (Type)
import           Data.Text (Text)
import           Database.Beam (Beamable, Columnar,
                                Table (PrimaryKey, primaryKey))
import           GHC.Generics (Generic)
import           Prelude hiding (id)

data BadSchemaT (f :: Type -> Type) = BadSchemaT
  { id  :: Columnar f Int64,
    dat :: Columnar f Text
  }
  deriving stock (Generic)
  deriving anyclass (Beamable)

instance Table BadSchemaT where
  data PrimaryKey BadSchemaT (f :: Type -> Type) =
    BadSchemaTPK (Columnar f Int64)
    deriving stock (Generic)
    deriving anyclass (Beamable)
  primaryKey = BadSchemaTPK . id
