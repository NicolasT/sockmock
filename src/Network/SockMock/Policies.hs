{-# LANGUAGE OverloadedStrings #-}

module Network.SockMock.Policies (
      blackhole
    , disconnect
    , disconnectLater
    ) where

import Control.Concurrent (threadDelay)

import qualified Data.Text as T

import Pipes
import qualified Pipes.Prelude as PP

import Network.SockMock (Application, logMessage, pipeHandler)

blackhole :: Application
blackhole prod cons = do
    logMessage "Blackholing connection"
    liftIO $ runEffect $ prod >-> PP.drain >-> cons

disconnect :: Application
disconnect = pipeHandler (return ())

disconnectLater :: Int -> Application
disconnectLater d _ _ = do
    logMessage msg
    liftIO $ threadDelay d
    logMessage "Disconnecting"
  where
    msg = T.concat [ "Disconnecting after "
                   , T.pack $ show d
                   , "us"
                   ]
