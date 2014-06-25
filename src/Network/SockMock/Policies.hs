{-# LANGUAGE OverloadedStrings #-}

module Network.SockMock.Policies (
      blackhole
    , disconnect
    ) where

import qualified Pipes.Prelude as PP

import Network.SockMock (Application, logMessage)

blackhole :: Application ()
blackhole = do
    logMessage "Blackholing connection"
    PP.drain

disconnect :: Application ()
disconnect = logMessage "Disconnecting"
