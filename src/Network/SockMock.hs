{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Network.SockMock (
      Handler
    , Application
    , HostPreference(..)
    , pipeHandler
    , logMessage
    , remoteAddress
    , tcpServer
    , tcpServer'
    , tlsServer
    , tlsServer'
    , run
    ) where

import Control.Monad

import Control.Applicative (Applicative)

import Control.Concurrent (forkIO, threadDelay)

import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.Trans.Reader (ReaderT(runReaderT))

import Control.Exception (finally)

import Control.Lens (makeLenses, view)

import Data.Typeable (Typeable)

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

data HandlerState = HandlerState { _remoteAddress :: !SockAddr
                                 , _enableLogging :: !Bool
                                 }
  deriving (Show, Eq, Typeable)

makeLenses ''HandlerState

newtype Handler r = Handler { runHandler :: ReaderT HandlerState IO r }
  deriving (Functor, Applicative, Monad, MonadReader HandlerState, MonadIO)

type Application = Producer ByteString IO () -> Consumer ByteString IO () -> Handler ()

data Server = TCPServer HostPreference ServiceName (Maybe Int) (Maybe Int) Application
            | TLSServer ServerSettings HostPreference ServiceName (Maybe Int) (Maybe Int) Application

logMessage :: Text -> Handler ()
logMessage msg = do
    verbose <- view enableLogging
    when verbose $ do
        addr <- view remoteAddress
        liftIO $ T.hPutStrLn stderr $ buildMessage addr
  where
    buildMessage addr = T.concat [ "["
                                 , T.pack $ show addr
                                 , "] "
                                 , msg
                                 ]

bufferSize :: Int
bufferSize = 4096

pipeHandler :: Pipe ByteString ByteString IO () -> Application
pipeHandler p = \prod cons -> liftIO $ runEffect $ prod >-> p >-> cons

runApp :: Application
       -> SockAddr
       -> Bool
       -> Producer ByteString IO ()
       -> Consumer ByteString IO ()
       -> IO ()
runApp app addr verbose prod cons = do
    finally
        (flip runReaderT state $ runHandler $ do
            logMessage "Connected"
            app prod cons)
        (flip runReaderT state $ runHandler $ logMessage "Disconnected")
  where
    state = HandlerState { _remoteAddress = addr
                         , _enableLogging = verbose
                         }

runTCP :: MonadSafe m
       => HostPreference
       -> ServiceName
       -> Maybe Int
       -> Maybe Int
       -> Bool
       -> Application
       -> m ()
runTCP host service readTimeout sendTimeout verbose app =
    PNS.serve host service $ \(sock, addr) -> do
        let prod = maybe PN.fromSocket PN.fromSocketTimeout readTimeout sock bufferSize
            cons = maybe PN.toSocket PN.toSocketTimeout sendTimeout sock

        runApp app addr verbose prod cons

runTLS :: (MonadSafe m, Base m ~ IO)
       => ServerSettings
       -> HostPreference
       -> ServiceName
       -> Maybe Int
       -> Maybe Int
       -> Bool
       -> Application
       -> m ()
runTLS config host service readTimeout sendTimeout verbose app =
    PNTS.serve config host service $ \(ctx, addr) -> do
        let prod = maybe PNT.fromContext PNT.fromContextTimeout readTimeout ctx
            cons = maybe PNT.toContext PNT.toContextTimeout sendTimeout ctx

        runApp app addr verbose prod cons

tcpServer :: ServiceName -> Application -> Server
tcpServer n = TCPServer PNS.HostAny n Nothing Nothing

tcpServer' :: HostPreference
           -> ServiceName
           -> Maybe Int
           -> Maybe Int
           -> Application
           -> Server
tcpServer' = TCPServer

tlsServer :: ServerSettings -> ServiceName -> Application -> Server
tlsServer c n = TLSServer c PNS.HostAny n Nothing Nothing

tlsServer' :: ServerSettings
           -> HostPreference
           -> ServiceName
           -> Maybe Int
           -> Maybe Int
           -> Application
           -> Server
tlsServer' = TLSServer

run :: Bool -> [Server] -> IO ()
run verbose servers = do
    forM_ servers $ \case
        TCPServer h s r w a -> fork (runTCP h s r w verbose a)
        TLSServer c h s r w a -> fork (runTLS c h s r w verbose a)
    forever $ threadDelay 1000000
  where
    fork = forkIO . runSafeT
