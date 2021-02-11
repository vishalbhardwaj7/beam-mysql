{-# LANGUAGE DeriveAnyClass #-}

module DB (
  TestDB(..),
  testDB
  ) where

import           DB.Bobby (BobbyT)
import qualified DB.Bobby as Bobby
import           DB.Latin1 (Latin1T)
import qualified DB.Latin1 as Latin1
import           DB.Nullable (NullableT)
import qualified DB.Nullable as Nullable
import           DB.PrimaryKey.AutoInc (AutoIncT)
import qualified DB.PrimaryKey.AutoInc as AutoInc
import           DB.PrimaryKey.NoAutoInc (NoAutoIncT)
import qualified DB.PrimaryKey.NoAutoInc as NoAutoInc
import           DB.PrimaryKey.None (NoneT)
import qualified DB.PrimaryKey.None as None
import           DB.Unicode (UnicodeT)
import qualified DB.Unicode as Unicode
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
  bobby    :: f (TableEntity BobbyT),
  unicode  :: f (TableEntity UnicodeT),
  pkNoAi   :: f (TableEntity NoAutoIncT),
  pkAi     :: f (TableEntity AutoIncT),
  noPk     :: f (TableEntity NoneT),
  latin1   :: f (TableEntity Latin1T)
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
                                    Bobby.badText2 = fieldNamed "bad_text2" },
      unicode = modifyTableFields $
                  tableModification { Unicode.id = fieldNamed "id",
                                      Unicode.dat = fieldNamed "data" },
      pkNoAi = modifyTableFields $
                  tableModification { NoAutoInc.id = fieldNamed "id",
                                      NoAutoInc.dat = fieldNamed "data" },
      pkAi = modifyTableFields $
                  tableModification { AutoInc.id = fieldNamed "id",
                                      AutoInc.dat = fieldNamed "data" },
      noPk = modifyTableFields $
                  tableModification { None.id = fieldNamed "id",
                                      None.dat = fieldNamed "data" },
      latin1 = modifyTableFields $
                  tableModification { Latin1.id = fieldNamed "id",
                                      Latin1.dat = fieldNamed "data" }
      }
    names :: DatabaseModification (DatabaseEntity MySQL TestDB) MySQL TestDB
    names = (dbModification @_ @MySQL) {
      nullable = setEntityName "nullable",
      viaJson = setEntityName "via_json",
      bobby = setEntityName "bobby",
      unicode = setEntityName "unicode",
      pkNoAi = setEntityName "pk_no_ai",
      pkAi = setEntityName "pk_ai",
      noPk = setEntityName "no_pk",
      latin1 = setEntityName "latin1"
      }
