{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.ByteString.Char8 as BS8

import Pipes

import Network.SockMock (Application, remoteAddress, run, tcpServer)
import Network.SockMock.Policies (blackhole, disconnect, disconnectLater)

sayHello :: Application ()
sayHello = do
    addr <- remoteAddress
    yield $ BS8.concat [ "Hello, "
                       , BS8.pack $ show addr
                       , "\n"
                       ]

main :: IO ()
main = run servers
  where
    servers = [ tcpServer "8080" disconnect
              , tcpServer "8081" blackhole
              , tcpServer "8082" sayHello
              , tcpServer "8083" (disconnectLater 1000000)
              ]
