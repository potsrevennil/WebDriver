
{-# LANGUAGE DeriveGeneric, OverloadedStrings, RecordWildCards, NamedFieldPuns, FlexibleContexts #-}
module Commands.Internal where

import Network.HTTP.Client
import Network.HTTP.Types
import Network.HTTP.Types.Header
import Data.Typeable
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.ByteString (ByteString, append)
import Data.Aeson
import Data.Aeson.Types
import Data.Attoparsec.ByteString.Lazy (Result(..))
import qualified Data.Attoparsec.ByteString.Lazy as AP
import qualified Data.ByteString.Lazy as LB


import Control.Monad.Trans.State.Lazy
import Control.Monad.Base
import Control.Monad (void)
import Control.Exception.Lifted (throwIO)
import Control.Exception (Exception, SomeException(..), toException)
import GHC.Generics

import Sessions
import Data.LocationStrategy

import Data.Aeson.Text
import Control.Lens hiding ((.=))
import Data.Aeson.Lens

instance Exception BadJSON
newtype BadJSON = BadJSON String
             deriving (Eq, Show, Typeable)

parseAesonResult :: (MonadBase IO m, FromJSON a) => Data.Aeson.Types.Result a -> m a
parseAesonResult (Success val) = return val
parseAesonResult (Error err) = throwIO $ BadJSON err

parseByteString :: (MonadBase IO m, FromJSON a) => LB.ByteString -> m a
parseByteString s = case AP.parse json s of
                Done _ res -> parseAesonResult (fromJSON res)
                Fail _ _ err   -> throwIO $ BadJSON err


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
        path = path,
        method = meth,
        requestBody = RequestBodyLBS body
    }

sendRequest :: Request -> SessState (Response LB.ByteString)
sendRequest req = do
    s@Session {..} <- SessState get
    liftBase (httpLbs req sessManager)


data HTTPStatusUnknown = HTTPStatusUnknown Int ByteString deriving (Eq, Show, Typeable)
instance Exception HTTPStatusUnknown

updateSessionId :: Maybe Text -> SessState ()
updateSessionId Nothing = return ()
updateSessionId (Just sid) = do
    s <- SessState get
    SessState (put (s {sessId = SessionId sid}))

-- -- TODO : handle error status
-- parseResBodyStatus :: ResponseMes -> SessState (Maybe SomeException)
-- parseResBodyStatus ResponseMes {status = Just 0} = return Nothing
-- parseResBodyStatus _ = return Nothing

-- TODO : handle status other than 200
parseResponse :: (FromJSON a) => Response LB.ByteString -> SessState (Either SomeException a)
parseResponse res
    | status == 200 = 
        case body ^? key "value" of
            Nothing -> Right <$> (parseAesonResult . fromJSON $ Null)
            Just val -> do
                updateSessionId (body ^? key "sessionId" . _String) 
                Right <$> (parseAesonResult . fromJSON $ val)
    | otherwise =  return . Left . toException . HTTPStatusUnknown status . statusMessage . responseStatus $ res
        where
            status = statusCode (responseStatus res)
            body = responseBody res

doCommand :: (ToJSON a, FromJSON b) => ByteString -> Method -> a -> SessState b
doCommand path meth args = mkRequest path meth args >>= sendRequest >>= parseResponse >>= either throwIO return

doSessCommand :: (ToJSON a, FromJSON b) => ByteString -> Method -> a -> SessState b
doSessCommand path meth args = do 
    Session {sessId} <- SessState get
    doCommand ("session/" `append` encodeUtf8 (getSessionId sessId) `append` path) meth args

doSessWinCommand :: (ToJSON a, FromJSON b) => ByteString -> Method -> a -> SessState b
doSessWinCommand path = doSessCommand ("/window/" `append` path)

doSessElCommand :: (ToJSON a, FromJSON b) => Text -> ByteString -> Method -> a -> SessState b
doSessElCommand eid path = doSessCommand ("/element/" `append` encodeUtf8 eid `append` path) 

ignore :: SessState Value -> SessState ()
ignore = void

retValue :: SessState Value -> SessState Value
retValue = id