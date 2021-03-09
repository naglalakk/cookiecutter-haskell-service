{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Config where

import Control.Exception (throwIO)
import Control.Monad.Except
  ( ExceptT,
    MonadError,
  )
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Monad.Reader
  ( MonadIO,
    MonadReader,
    ReaderT,
  )
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
  ( MaybeT (..),
    runMaybeT,
  )
import qualified Data.ByteString.Char8 as BS
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Database.Bloodhound
  ( BHEnv,
    Server (..),
    mkBHEnv,
  )
import Database.Persist.Postgresql
  ( ConnectionPool,
    createPostgresqlPool,
  )
import Database.Redis
  ( Connection,
    connect,
    connectHost,
    defaultConnectInfo,
  )
import Network.HTTP.Client
  ( defaultManagerSettings,
    newManager,
  )
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.Server (ServerError)
import System.Environment (lookupEnv)
import System.Log.Raven (initRaven, stderrFallback)
import System.Log.Raven.Transport.HttpConduit (sendRecordWith)
import System.Log.Raven.Types (SentryService (..))
import Utils (lookupSetting)

newtype AppT m a
  = AppT
      { runApp :: ReaderT Config (ExceptT ServerError m) a
      }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader Config,
      MonadError ServerError,
      MonadIO
    )

type App = AppT IO

-- | The Config for our application
data Config
  = Config
      { configPool :: ConnectionPool,
        configEnv :: Environment,
        esEnv :: BHEnv,
        redisConnection :: Connection,
        sentryService :: Maybe SentryService
      }

-- | Environments
data Environment
  = Development
  | Test
  | Production
  deriving (Eq, Show, Read)

-- | Get default config
getConfig :: IO Config
getConfig = do
  env <- lookupSetting "ENV" Development
  pool <- makePool env
  es <- initES env
  cache <- initCache env
  sentry <- initSentry env
  return
    Config
      { configPool = pool,
        configEnv = env,
        esEnv = es,
        redisConnection = cache,
        sentryService = sentry
      }

makePool :: Environment -> IO ConnectionPool
makePool env = do
  pool <- runMaybeT $ do
    let keys = ["host=", " port=", " user=", " password=", " dbname="]
        envs = ["PGHOST", "PGPORT", "PGUSER", "PGPASS", "PGDATABASE"]
    envVars <- traverse (MaybeT . lookupEnv) envs
    let prodStr = BS.intercalate " " . zipWith (<>) keys $ BS.pack <$> envVars
    lift $ runStdoutLoggingT $ createPostgresqlPool prodStr (envPool env)
  case pool of
    -- If we don't have a correct database configuration, we can't
    -- handle that in the program, so we throw an IO exception. This is
    -- one example where using an exception is preferable to 'Maybe' or
    -- 'Either'.
    Nothing ->
      throwIO (userError "Database Configuration not present in environment.")
    Just a -> return a

-- | Init Redis Cache
initCache :: Environment -> IO Connection
initCache env = do
  case env of
    Production -> do
      host <- lookupEnv "REDIS_HOST"
      let connectInfo =
            defaultConnectInfo {connectHost = fromMaybe "localhost" host}
      connect connectInfo
    _ -> connect defaultConnectInfo

-- | Init Elasticsearch connection
initES :: Environment -> IO BHEnv
initES env = do
  manager <- newManager defaultManagerSettings
  host <- lookupEnv "ES_URL"
  let serverStr = T.pack $ fromMaybe "http://localhost:9200" host
  return $ mkBHEnv (Server serverStr) manager

initSentry :: Environment -> IO (Maybe SentryService)
initSentry env = case env of
  Production -> do
    sURL <- lookupEnv "SENTRY_URL"
    case sURL of
      Just url -> do
        http <- newManager tlsManagerSettings
        rManager <- initRaven url id (sendRecordWith http) stderrFallback
        return $ Just rManager
      Nothing -> return Nothing
  _ -> return Nothing

-- | The number of pools to use for a given environment.
envPool :: Environment -> Int
envPool Test = 1
envPool Development = 1
envPool Production = 8

-- | Default static filePath
filePath :: Environment -> FilePath
filePath Development = "static/uploads/"
filePath Production = ""
