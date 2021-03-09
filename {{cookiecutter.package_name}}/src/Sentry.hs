module Sentry where

import Config (Config (..), Environment (..))
import Control.Exception
  ( SomeException,
  )
import Data.ByteString.Char8 (unpack)
import Network.Wai (Request, rawPathInfo, requestHeaderHost)
import Network.Wai.Handler.Warp (defaultOnException)
import System.Log.Raven (register)
import System.Log.Raven.Types
  ( SentryLevel (Error),
    SentryRecord (..),
  )

sentryOnException :: Config -> Maybe Request -> SomeException -> IO ()
sentryOnException cfg req ex = do
  case configEnv cfg of
    Production ->
      case sentryService cfg of
        Just sService -> do
          register
            sService
            "sentry.logger"
            Error
            (formatMessage req ex)
            (recordUpdate req ex)
        Nothing -> return ()
    _ -> return ()
  defaultOnException req ex

formatMessage :: Maybe Request -> SomeException -> String
formatMessage Nothing exception = "Exception before request could be parsed: " ++ show exception
formatMessage (Just request) exception = "Exception " ++ show exception ++ " while handling request " ++ show request

recordUpdate :: Maybe Request -> SomeException -> SentryRecord -> SentryRecord
recordUpdate Nothing exception record = record
recordUpdate (Just request) exception record =
  record
    { srCulprit = Just $ unpack $ rawPathInfo request,
      srServerName = unpack <$> requestHeaderHost request
    }
