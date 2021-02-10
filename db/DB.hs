{-# LANGUAGE DeriveAnyClass #-}

module DB (
  TestDB(..),
  testDB
  ) where

import           DB.Bobby (BobbyT)
import qualified DB.Bobby as Bobby
import           DB.Nullable (NullableT)
import qualified DB.Nullable as Nullable
import           DB.ViaJSON (ViaJSONT)
import qualified DB.ViaJSON as ViaJSON
import           Data.Kind (Type)
import           Database.Beam (Database, DatabaseEntity, DatabaseModification,
                                DatabaseSettings, TableEntity, dbModification,
                                defaultDbSettings, fieldNamed,
                                modifyTableFields, setEntityName,
                                tableModification, withDbModification)
import           Database.Beam.MySQL (MySQL)
import           GHC.Generics (Generic)

data TestDB (f :: Type -> Type) = TestDB {
  nullable :: f (TableEntity NullableT),
  viaJson  :: f (TableEntity ViaJSONT),
  bobby    :: f (TableEntity BobbyT)
  }
  deriving stock (Generic)
  deriving anyclass (Database MySQL)

testDB :: DatabaseSettings MySQL TestDB
testDB = defaultDbSettings `withDbModification` fields `withDbModification` names
  where
    fields :: DatabaseModification (DatabaseEntity MySQL TestDB) MySQL TestDB
    fields = (dbModification @_ @MySQL) {
      nullable = modifyTableFields $
                  tableModification { Nullable.id = fieldNamed "id",
                                      Nullable.dat = fieldNamed "data" },
      viaJson = modifyTableFields $
                  tableModification { ViaJSON.id = fieldNamed "id",
                                      ViaJSON.fromBool = fieldNamed "from_bool",
                                      ViaJSON.fromDouble = fieldNamed "from_double",
                                      ViaJSON.fromString = fieldNamed "from_string",
                                      ViaJSON.fromArray = fieldNamed "from_array",
                                      ViaJSON.fromObject = fieldNamed "from_object" },
      bobby = modifyTableFields $
                tableModification { Bobby.id = fieldNamed "id",
                                    Bobby.badText = fieldNamed "bad_text",
                                    Bobby.badText2 = fieldNamed "bad_text2" }
      }
    names :: DatabaseModification (DatabaseEntity MySQL TestDB) MySQL TestDB
    names = (dbModification @_ @MySQL) {
      nullable = setEntityName "nullable",
      viaJson = setEntityName "via_json",
      bobby = setEntityName "bobby"
      }
