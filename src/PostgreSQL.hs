module PostgreSQL where

import qualified Configuration.Dotenv          as Environment
import qualified Database.Beam                 as Database
import qualified Database.Beam.Postgres        as Database
import qualified Database.PostgreSQL.Simple    as Database
import qualified System.Environment            as Environment
import qualified Text.Pretty.Simple            as Pretty
import           Universum
import qualified Universum.Unsafe              as Unsafe

type User = UserTable Identity
deriving instance Eq   User
deriving instance Show User

data UserTable f = User
  { name     :: Database.Columnar f Text
  , email    :: Database.Columnar f Text
  , password :: Database.Columnar f Text
  }
  deriving (Generic, Database.Beamable)

type UserId = Database.PrimaryKey UserTable Identity
deriving instance Eq   UserId
deriving instance Show UserId

instance Database.Table UserTable where
  data PrimaryKey UserTable f = UserId (Database.Columnar f Text) deriving (Generic, Database.Beamable)
  primaryKey = UserId . email

type Address = AddressTable Identity
deriving instance Eq   Address
deriving instance Show Address

data AddressTable f = AddressTable
  { addressId :: Database.Columnar f Int32
  , line1     :: Database.Columnar f Text
  , line2     :: Database.Columnar f (Maybe Text)
  , city      :: Database.Columnar f Text
  , state     :: Database.Columnar f Text
  , zip       :: Database.Columnar f Text
  , user      :: Database.PrimaryKey UserTable f
  }
  deriving (Generic, Database.Beamable)

type AddressId = Database.PrimaryKey AddressTable Identity
deriving instance Eq   AddressId
deriving instance Show AddressId

instance Database.Table AddressTable where
  data PrimaryKey AddressTable f = AddressId (Database.Columnar f Int32) deriving (Generic, Database.Beamable)
  primaryKey = AddressId . addressId

{- ****************************** -}

data Database f = Database
  { users     :: f (Database.TableEntity UserTable)
  , addresses :: f (Database.TableEntity AddressTable)
  }
  deriving (Generic, Database.Database Database.Postgres)

database :: Database.DatabaseSettings Database.Postgres Database
database = Database.defaultDbSettings `Database.withDbModification` Database.dbModification
  { addresses = Database.setEntityName "user_addresses"
                  <> Database.modifyTableFields Database.tableModification { addressId = "id" }
  }

{- ****************************** -}

connectDatabase :: IO Database.Connection
connectDatabase = do
  Environment.loadFile Environment.defaultConfig
  let environmentVariables = ["DB_HOST", "DB_PORT", "DB_USER", "DB_NAME", "DB_PASSWORD"] :: Vector String
  [host, port, user, dbname, password] <- mapM Environment.getEnv environmentVariables
  Database.connect (Database.ConnectInfo host (fromMaybe 5432 (readMaybe port)) user password dbname)

start :: IO ()
start = bracket
  connectDatabase
  Database.close
  \connection -> do
    -- runActionWithDebug connection
    --   $ (Database.runInsert . Database.insert (users database) . Database.insertValues)
    --       [ User "Dicky7"  "dicky7@gmail.com"  "123"
    --       , User "Dicky8"  "dicky8@gmail.com"  "123"
    --       , User "Dicky9"  "dicky9@gmail.com"  "123"
    --       , User "Dicky10" "dicky10@gmail.com" "123"
    --       , User "Dicky11" "dicky11@gmail.com" "123"
    --       , User "Dicky12" "dicky12@gmail.com" "123"
    --       ]
    -- runActionWithDebug
    --   connection
    --   do
    --     users <-
    --       Database.runSelectReturningList
    --       $ Database.select
    --       $ Database.limit_ 1
    --       $ Database.offset_ 1
    --       $ Database.orderBy_ (Database.desc_ . name)
    --       $ Database.all_ (users database)
    runActionWithDebug
      connection
      do
        users <- Database.runSelectReturningList $ Database.select $ Database.aggregate_
          (\User {..} -> (Database.group_ name, Database.as_ @Int32 Database.countAll_))
          (Database.all_ (users database))
        Pretty.pPrint users

runActionWithDebug :: Database.Connection -> Database.Pg () -> IO ()
runActionWithDebug = Database.runBeamPostgresDebug Pretty.pPrintString

actionInsertUserTable :: Database.MonadBeam Database.Postgres IO => [User] -> IO ()
actionInsertUserTable = Database.runInsert . Database.insert (users database) . Database.insertValues

-- actionBoundedQueryUserTable :: Database.MonadBeam Database.Postgres IO => IO()
-- actionBoundedQueryUserTable = 
