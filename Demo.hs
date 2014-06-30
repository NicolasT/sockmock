{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.ByteString.Char8 as BS8

import Control.Lens (view)

import Pipes

import Network.SockMock (Application, remoteAddress, run, tcpServer)
import Network.SockMock.Policies
import Network.SockMock.HTTP (HTTPApplication, Response(..), http, http10)

sayHello :: Application
sayHello _ cons = do
    addr <- view remoteAddress
    liftIO $ runEffect $ yield (message addr) >-> cons
  where
    message addr = BS8.concat [ "Hello, "
                              , BS8.pack $ show addr
                              , "\n"
                              ]

httpApp :: HTTPApplication
httpApp _req = return (response, yield msg)
  where
    msg = "Hello, world!"
    len = BS8.length msg
    headers = [ ("Content-Length", BS8.pack $ show len)
              , ("Content-Type", "text/plain")
              ]
    response = Response { rpCode = 200
                        , rpHeaders = headers
                        , rpVersion = http10
                        , rpMessage = "OK"
                        }

main :: IO ()
main = run False servers
  where
    servers = [ tcpServer "8080" disconnect
              , tcpServer "8081" blackhole
              , tcpServer "8082" sayHello
              , tcpServer "8083" (disconnectLater 1000000)
              , tcpServer "8084" (tcpProxy "127.0.0.1" "8082")
              , tcpServer "8085" (tcpProxyTimeout "127.0.0.1" "8081" 5000000)
              , tcpServer "8086" echo
              , tcpServer "8087" (http httpApp)
              ]
