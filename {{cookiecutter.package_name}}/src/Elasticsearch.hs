{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Elasticsearch where

import Config
  ( Config,
    esEnv,
  )
import Control.Monad.Reader
  ( MonadIO,
    MonadReader,
    asks,
  )
import Data.Aeson
import qualified Data.Text as T
import Database.Bloodhound
import Network.HTTP.Client (responseStatus)
import Network.HTTP.Types (statusCode)

indexSettings :: IndexSettings
indexSettings = defaultIndexSettings

makeIndex :: (MonadReader Config m, MonadIO m) => IndexName -> m Reply
makeIndex name = runES $ createIndex indexSettings name

destroyIndex :: (MonadReader Config m, MonadIO m) => IndexName -> m Reply
destroyIndex name = runES $ deleteIndex name

makeMapping ::
  (MonadReader Config m, MonadIO m, ToJSON a) =>
  IndexName ->
  MappingName ->
  a ->
  m Reply
makeMapping ix mn mp = runES $ putMapping ix mn mp

updateOrCreate ::
  (MonadReader Config m, MonadIO m, ToJSON a) =>
  IndexName ->
  MappingName ->
  a ->
  T.Text ->
  m Reply
updateOrCreate ixn mn object dID = do
  reply <- getRecord ixn mn dID
  let sCode = statusCode $ responseStatus reply
  case sCode of
    -- update
    200 -> updateRecord ixn mn object dID
    -- create
    404 ->
      runES $ indexDocument ixn mn defaultIndexDocumentSettings object $
        DocId
          dID

updateRecord ::
  (MonadReader Config m, MonadIO m, ToJSON a) =>
  IndexName ->
  MappingName ->
  a ->
  T.Text ->
  m Reply
updateRecord ixn mn object dID =
  runES $ updateDocument ixn mn defaultIndexDocumentSettings object $ DocId dID

getRecord ::
  (MonadReader Config m, MonadIO m) =>
  IndexName ->
  MappingName ->
  T.Text ->
  m Reply
getRecord ixn mn dID = runES $ getDocument ixn mn (DocId dID)

runES :: (MonadReader Config m, MonadIO m) => BH m a -> m a
runES query = do
  bhEnv <- asks esEnv
  runBH bhEnv query
