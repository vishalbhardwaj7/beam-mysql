{-# LANGUAGE DeriveAnyClass #-}

module DB (
  TestDB(..),
  testDB
  ) where

import           DB.BadSchema (BadSchemaT)
import qualified DB.BadSchema as BadSchema
import           DB.BadSchemaBig (BadSchemaBigT)
import qualified DB.BadSchemaBig as BadSchemaBig
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
  nullable     :: f (TableEntity NullableT),
  viaJson      :: f (TableEntity ViaJSONT),
  bobby        :: f (TableEntity BobbyT),
  unicode      :: f (TableEntity UnicodeT),
  pkNoAi       :: f (TableEntity NoAutoIncT),
  pkAi         :: f (TableEntity AutoIncT),
  noPk         :: f (TableEntity NoneT),
  latin1       :: f (TableEntity Latin1T),
  badSchema    :: f (TableEntity BadSchemaT),
  badSchemaBig :: f (TableEntity BadSchemaBigT)
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
                                      Latin1.dat = fieldNamed "data" },
      badSchema = modifyTableFields $
                    tableModification { BadSchema.id = fieldNamed "id",
                                        BadSchema.dat = fieldNamed "data" },
      badSchemaBig = modifyTableFields $
                      tableModification { BadSchemaBig.id = fieldNamed "id",
                                          BadSchemaBig.field1 = fieldNamed "field_1",
                                          BadSchemaBig.field2 = fieldNamed "field_2",
                                          BadSchemaBig.field3 = fieldNamed "field_3",
                                          BadSchemaBig.field4 = fieldNamed "field_4" }
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
      latin1 = setEntityName "latin1",
      badSchema = setEntityName "bad_schema",
      badSchemaBig = setEntityName "bad_schema_big"
      }
