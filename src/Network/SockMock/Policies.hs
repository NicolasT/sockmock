{-# LANGUAGE OverloadedStrings #-}

module Network.SockMock.Policies (
      blackhole
    , disconnect
    , disconnectLater
    , tcpProxy
    ) where

import Control.Concurrent (forkIO, threadDelay)

import qualified Data.Text as T

import Pipes
import qualified Pipes.Prelude as PP

import Pipes.Network.TCP.Safe

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

tcpProxy :: HostName -> ServiceName -> Application
tcpProxy host service prod cons = do
    logMessage msg
    liftIO $ runSafeT $
        connect host service (\(sock, _) -> liftIO $ do
            _ <- forkIO $ runEffect $ fromSocket sock bufferSize >-> cons
            runEffect $ prod >-> toSocket sock)
  where
    bufferSize = 4096
    msg = T.concat [ "Proxying to "
                   , T.pack host
                   , ":"
                   , T.pack service
                   ]
