module PostgreSQL where

import qualified Configuration.Dotenv          as Environment
import qualified Database.Beam                 as Database
import qualified Database.Beam.Postgres        as Database
import qualified Database.PostgreSQL.Simple    as Database
import qualified System.Environment            as Environment
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

instance Database.Table UserTable where
  data PrimaryKey UserTable f = UserId (Database.Columnar f Text) deriving (Generic, Database.Beamable)
  primaryKey = UserId . email

{- ****************************** -}

data Database f = Database
  { users :: f (Database.TableEntity UserTable)
  }
  deriving (Generic, Database.Database Database.Postgres)

database :: Database.DatabaseSettings Database.Postgres Database
database = Database.defaultDbSettings

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
  \connection ->
    Database.runBeamPostgresDebug putStrLn connection
      $ Database.runInsert
      $ Database.insert (users database)
      $ Database.insertValues [User "dicky1@gmail.com" "Dicky1" "123"]


