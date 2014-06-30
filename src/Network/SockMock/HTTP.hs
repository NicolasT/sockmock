{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.SockMock.HTTP (
      module Network.Types
    , HTTPApplication
    , http
    ) where

import Data.Monoid

import Data.ByteString (ByteString)
import Data.ByteString.Builder (byteString, char7, intDec, toLazyByteString)

import Pipes
import Pipes.Parse (runStateT)
import Pipes.Attoparsec (parse)
import Pipes.ByteString (fromLazy)

import Network.Types (HttpVersion(..), Method(..), Request(..), RequestUri(..),
    Response(..), URI(..), URIAuth(..), http10, http11)
import Network.Parser.Rfc2616 (request)

import Network.SockMock

type HTTPApplication = Request -> IO (Response, Producer ByteString IO ())

http :: HTTPApplication -> Application
http app prod cons = do
    c <- liftIO $ do
        (req, prod') <- runStateT (parse request) prod
        case req of
            Nothing -> return $ Left ()
            Just (Left e) -> fail $ show e -- TODO
            Just (Right req') -> do
                (res, body) <- app req'
                runEffect $ do
                    responseProducer res >-> cons
                    body >-> cons
                return $ Right prod'

    case c of
        Left () -> return ()
        Right prod' -> http app prod' cons

responseProducer :: Monad m => Response -> Producer ByteString m ()
responseProducer Response{..} = fromLazy $ toLazyByteString builder
  where
    builder = status <> headers <> newline
    version = byteString "HTTP/" <> intDec (httpMajor rpVersion)
                                 <> char7 '.'
                                 <> intDec (httpMinor rpVersion)
    status = mconcat [ version
                     , space
                     , intDec rpCode
                     , space
                     , byteString rpMessage
                     , newline
                     ]
    headers = mconcat $ map
                (\(k, v) -> byteString k <> byteString ": " <> byteString v <> newline)
                rpHeaders

    newline = char7 '\n'
    space = char7 ' '
