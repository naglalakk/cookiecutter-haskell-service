{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Reader                   (runReaderT)
import Database.Persist.Postgresql            (runSqlPool)
import Network.Wai.Handler.Warp               (run)
import System.Environment                     (lookupEnv)
import Safe                                   (readMay)
import Configuration.Dotenv.Types             (defaultConfig)
import Network.Wai                            (Middleware)
import Network.Wai.Middleware.Cors
import qualified Configuration.Dotenv as Dotenv
import System.Log.Logger
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple
import System.Log.Formatter


import Api                                    (app)
import Config                                 (Config (..), Environment (..),
                                               makePool, setLogger, initES)
import Models                                 (doMigrations)
import Elasticsearch                          (syncDb$moduleNames)

-- | Allow Content-Type header with values other then allowed by simpleCors.
corsWithContentType :: Middleware
corsWithContentType = cors (const $ Just policy)
    where
      policy = simpleCorsResourcePolicy
        { corsRequestHeaders = ["Content-Type"] }

-- | The 'main' function gathers the required environment information and
-- initializes the application.
main :: IO ()
main = do
    Dotenv.loadFile defaultConfig

    -- Set up log file handler
    updateGlobalLogger "$moduleName"
                       (setLevel DEBUG)
    h <- fileHandler "logs/server.log" DEBUG >>= \lh -> return $
         setFormatter lh (simpleLogFormatter "[$time : $loggername : $prio] $msg")
    updateGlobalLogger "$moduleName" (addHandler h)

    env  <- lookupSetting "ENV" Development
    port <- lookupSetting "PORT" 8081
    esEnv <- initES env
    pool <- makePool env
    let cfg = Config { configPool = pool
                     , configEnv = env
                     , esEnv = esEnv }
        logger = setLogger env

    -- Database migration
    runSqlPool doMigrations pool

    -- Sync Elasticsearch
    runReaderT syncDbRegistrations cfg

    run port $ logger $ corsWithContentType $ app cfg

-- | Looks up a setting in the environment, with a provided default, and
-- 'read's that information into the inferred type.
lookupSetting :: Read a => String -> a -> IO a
lookupSetting env def = do
    maybeValue <- lookupEnv env
    case maybeValue of
        Nothing ->
            return def
        Just str ->
            maybe (handleFailedRead str) return (readMay str)
  where
    handleFailedRead str =
        error $ mconcat
            [ "Failed to read [["
            , str
            , "]] for environment variable "
            , env
            ]
