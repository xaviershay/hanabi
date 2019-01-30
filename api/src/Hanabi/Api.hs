{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Hanabi.Api
  ( app
  ) where

import Hanabi.Types

import           Network.Wai.Middleware.Cors          (cors, corsRequestHeaders,
                                                       simpleCorsResourcePolicy)
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Servant
import           Control.Monad.Trans.Reader           (ReaderT, ask, runReaderT)

type AppM = ReaderT State Handler
type MyAPI = "_status" :> Get '[JSON] ()

nt :: State -> AppM a -> Handler a
nt s x = runReaderT x s

api :: Proxy MyAPI
api = Proxy

app :: State -> Application
app s =   logStdoutDev
        $ cors (const . Just $ corsPolicy)
        $ serve api
        $ hoistServer api (nt s) server
  where
    corsPolicy = simpleCorsResourcePolicy
                   { corsRequestHeaders = [ "authorization", "content-type" ]
                   }

server = getStatus

getStatus :: AppM ()
getStatus = return ()
