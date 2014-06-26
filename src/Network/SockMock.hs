{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.SockMock (
      Application
    , HostPreference(..)
    , logMessage
    , remoteAddress
    , tcpServer
    , tcpServer'
    , tlsServer
    , tlsServer'
    , run
    ) where

import Control.Monad

import Control.Concurrent (forkIO, threadDelay)

import Control.Monad.Reader.Class (ask)
import Control.Monad.Trans.Reader (ReaderT(runReaderT))

import Control.Exception (finally)

import Data.ByteString (ByteString)

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T (hPutStrLn)

import System.IO (stderr)

import Pipes

import Pipes.Safe (Base, MonadSafe, runSafeT)

import Pipes.Network.TCP (HostPreference, ServiceName, SockAddr)
import qualified Pipes.Network.TCP as PN
import qualified Pipes.Network.TCP.Safe as PNS

import Network.Simple.TCP.TLS (ServerSettings)

import qualified Pipes.Network.TCP.TLS as PNT
import qualified Pipes.Network.TCP.TLS.Safe as PNTS

type ApplicationConfig = SockAddr
type Application r = Pipe ByteString ByteString (ReaderT ApplicationConfig IO) r

data Server = TCPServer HostPreference ServiceName (Maybe Int) (Maybe Int) (Application ())
            | TLSServer ServerSettings HostPreference ServiceName (Maybe Int) (Maybe Int) (Application ())

buildMessage :: ApplicationConfig -> Text -> Text
buildMessage cfg msg = T.concat [ "["
                                , T.pack $ show cfg
                                , "] "
                                , msg
                                ]

remoteAddress :: Application SockAddr
remoteAddress = ask

logMessage :: Text -> Application ()
logMessage msg = do
    addr <- ask
    liftIO $ T.hPutStrLn stderr $ buildMessage addr msg

bufferSize :: Int
bufferSize = 4096

runApp :: Application ()
       -> ApplicationConfig
       -> Producer ByteString (ReaderT ApplicationConfig IO) ()
       -> Consumer ByteString (ReaderT ApplicationConfig IO) ()
       -> IO ()
runApp app addr prod cons = do
    T.hPutStrLn stderr $ buildMessage addr "Connected"
    finally
        (flip runReaderT addr $ runEffect $ prod >-> app >-> cons)
        (T.hPutStrLn stderr $ buildMessage addr "Disconnected")

runTCP :: MonadSafe m
       => HostPreference
       -> ServiceName
       -> Maybe Int
       -> Maybe Int
       -> Application ()
       -> m ()
runTCP host service readTimeout sendTimeout app =
    PNS.serve host service $ \(sock, addr) -> do
        let prod = maybe PN.fromSocket PN.fromSocketTimeout readTimeout sock bufferSize
            cons = maybe PN.toSocket PN.toSocketTimeout sendTimeout sock

        runApp app addr prod cons

runTLS :: (MonadSafe m, Base m ~ IO)
       => ServerSettings
       -> HostPreference
       -> ServiceName
       -> Maybe Int
       -> Maybe Int
       -> Application ()
       -> m ()
runTLS config host service readTimeout sendTimeout app =
    PNTS.serve config host service $ \(ctx, addr) -> do
        let prod = maybe PNT.fromContext PNT.fromContextTimeout readTimeout ctx
            cons = maybe PNT.toContext PNT.toContextTimeout sendTimeout ctx

        runApp app addr prod cons

tcpServer :: ServiceName -> Application () -> Server
tcpServer n = TCPServer PNS.HostAny n Nothing Nothing

tcpServer' :: HostPreference
           -> ServiceName
           -> Maybe Int
           -> Maybe Int
           -> Application ()
           -> Server
tcpServer' = TCPServer

tlsServer :: ServerSettings -> ServiceName -> Application () -> Server
tlsServer c n = TLSServer c PNS.HostAny n Nothing Nothing

tlsServer' :: ServerSettings
           -> HostPreference
           -> ServiceName
           -> Maybe Int
           -> Maybe Int
           -> Application ()
           -> Server
tlsServer' = TLSServer

run :: [Server] -> IO ()
run servers = do
    forM_ servers $ \case
        TCPServer h s r w a -> fork (runTCP h s r w a)
        TLSServer c h s r w a -> fork (runTLS c h s r w a)
    forever $ threadDelay 1000000
  where
    fork = forkIO . runSafeT
