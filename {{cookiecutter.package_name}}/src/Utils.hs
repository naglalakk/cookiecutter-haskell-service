module Utils where

import Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Lazy as BL
import Data.IORef
  ( modifyIORef',
    newIORef,
    readIORef,
  )
import Network.Wai (Response, responseToStream)
import Safe (readMay)
import System.Environment (lookupEnv)

responseBody :: Response -> IO BL.ByteString
responseBody res =
  let (status, headers, body) = responseToStream res
   in body $ \f -> do
        content <- newIORef mempty
        f (\chunk -> modifyIORef' content (<> chunk)) (return ())
        toLazyByteString <$> readIORef content

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
      error $
        mconcat
          [ "Failed to read [[",
            str,
            "]] for environment variable ",
            env
          ]
