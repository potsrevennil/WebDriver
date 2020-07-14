
{-# LANGUAGE OverloadedStrings, RecordWildCards, NamedFieldPuns, FlexibleContexts #-}
module Commands.Internal where

import Network.HTTP.Client
import Network.HTTP.Types
import Network.HTTP.Types.Header
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.UTF8 as B
import Data.List

import Data.Aeson
import Data.Aeson.Types
import Data.Attoparsec.ByteString.Lazy (Result(..))
import qualified Data.Attoparsec.ByteString.Lazy as AP

import Control.Monad.Trans.State.Lazy
import Control.Monad.Base
import Control.Monad (void)
import Control.Exception.Lifted (throwIO)
import Control.Exception (SomeException(..), toException)
import Control.Monad.Catch

import Sessions
import Data.LocationStrategy
import Exceptions

import Control.Lens hiding ((.=))
import Data.Aeson.Lens

parseAesonResult :: (MonadBase IO m, FromJSON a) => Data.Aeson.Types.Result a -> m a
parseAesonResult (Success val) = return val
parseAesonResult (Error err) = throwIO $ BadJSON err

parseByteString :: (MonadBase IO m, FromJSON a) => ByteString -> m a
parseByteString s = case AP.parse json s of
                Done _ res -> parseAesonResult (fromJSON res)
                Fail _ _ err   -> throwIO $ BadJSON (err ++ B.toString s)


mkRequest :: (ToJSON a) => ByteString -> Method -> a -> SessState Request
mkRequest path meth args = 
    let body = case toJSON args of 
                Null -> ""
                val  -> encode val in
    return defaultRequest {
        host = "127.0.0.1",
        port = 4444,
        secure = False,
        requestHeaders = [
                            (hAccept, "application/json;charset=utf-8"),
                            (hAcceptEncoding, "gzip,deflate"),
                            (hAcceptLanguage, "en-US,en;q=0.5"),
                            (hConnection, "keep-alive"),
                            (hContentType, "text/plain;charset=UTF-8")
                        ],
        path = B.toStrict path,
        method = meth,
        requestBody = RequestBodyLBS body
    }


updateSessionId :: Maybe Text -> SessState ()
updateSessionId Nothing = return ()
updateSessionId (Just sid) = do
    s <- SessState get
    SessState (put (s {sessId = SessionId sid}))

-- TODO : Error handling, Error msgs
sendRequest :: (FromJSON a) => Request -> SessState a
sendRequest req = do
    s@Session {..} <- SessState get
    res <- liftBase (httpLbs req sessManager)
    let parseResponse 
            -- status >= 100, < 200
            | statusIsInformational s = 
                throwM UnknownError
            -- status >= 200, < 300
            | statusIsSuccessful s = 
                case body ^? key "value" of
                    Nothing -> 
                        parseAesonResult . fromJSON $ Null
                    Just val -> do
                        updateSessionId (T.fromStrict <$> (body ^? key "sessionId" . _String) )
                        parseAesonResult . fromJSON $ val
            -- status >= 300, < 400
            | statusIsRedirection s = 
                case lookup hLocation headers of
                    Nothing -> throwM $ HttpExceptionRequest req (InvalidHeader "Cannot find redirect url.")
                    Just url -> parseRequest (B.toString ("GET " `B.append` B.fromStrict url)) >>= sendRequest
            -- status >= 400, < 500
            | statusIsClientError s =
                case body ^? key "value" of
                    Nothing -> 
                        throwM . HttpExceptionRequest req $ ProxyConnectException (B.toStrict . B.fromString $ show res) (statusCode s) s
                    Just val -> 
                        throwM . HttpExceptionRequest req $ ProxyConnectException (B.toStrict . encode $ val) (statusCode s) s     
            | statusIsServerError s =
                throwM $ toErrorCode (body ^? key "status" . _Integer, T.fromStrict <$> body ^? key "state" . _String)
            | otherwise =  
                throwM . HttpExceptionRequest req $ ProxyConnectException "Unkwon Error." (statusCode s) s
                where
                    s = responseStatus res
                    body = responseBody res
                    headers = responseHeaders res
    parseResponse

doCommand :: (ToJSON a, FromJSON b) => ByteString -> Method -> a -> SessState b
doCommand path meth args = mkRequest path meth args >>= sendRequest

doSessCommand :: (ToJSON a, FromJSON b) => ByteString -> Method -> a -> SessState b
doSessCommand path meth args = do 
    Session {sessId} <- SessState get
    doCommand ("session/" `B.append` T.encodeUtf8 (getSessionId sessId) `B.append` path) meth args

doSessWinCommand :: (ToJSON a, FromJSON b) => ByteString -> Method -> a -> SessState b
doSessWinCommand path = doSessCommand ("/window/" `B.append` path)

doSessElCommand :: (ToJSON a, FromJSON b) => Text -> ByteString -> Method -> a -> SessState b
doSessElCommand eid path = doSessCommand ("/element/" `B.append` T.encodeUtf8 eid `B.append` path) 

ignore :: SessState Value -> SessState ()
ignore = void

retValue :: SessState Value -> SessState Value
retValue = id