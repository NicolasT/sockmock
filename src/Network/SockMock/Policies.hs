{-# LANGUAGE OverloadedStrings #-}

module Network.SockMock.Policies (
      blackhole
    , disconnect
    , disconnectLater
    ) where

import Control.Concurrent (threadDelay)

import Control.Monad.IO.Class (liftIO)

import qualified Data.Text as T

import qualified Pipes.Prelude as PP

import Network.SockMock (Application, logMessage)

blackhole :: Application ()
blackhole = do
    logMessage "Blackholing connection"
    PP.drain

disconnect :: Application ()
disconnect = logMessage "Disconnecting"

disconnectLater :: Int -> Application ()
disconnectLater d = do
    logMessage $ T.append "Disconnecting after " t
    liftIO $ threadDelay d
    logMessage "Disconnecting"
  where
    t = T.append (T.pack $ show d) "us"
