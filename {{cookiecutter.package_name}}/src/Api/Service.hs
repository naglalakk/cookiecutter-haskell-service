{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DeriveGeneric     #-}

module Api.Service where

import Servant
import Control.Monad.IO.Class      (MonadIO)

import Api.User                    (UserAPI
                                   ,userServer)
import Config                      (AppT (..))

type ServiceAPI = UserAPI

serviceAPI :: Proxy ServiceAPI
serviceAPI = Proxy

serviceServer :: MonadIO m => ServerT ServiceAPI (AppT m)
serviceServer = userServer

