
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
import Control.Exception.Lifted (throwIO)
import Control.Exception (Exception, SomeException(..), toException)
import GHC.Generics

import Sessions
import Data.LocationStrategy

data ResponseMes = ResponseMes {
        sessionId :: Maybe SessionId,
        status :: Maybe Int,
        value :: Value
    } deriving (Eq, Show, Generic)

instance FromJSON ResponseMes where
    parseJSON = genericParseJSON defaultOptions
        {omitNothingFields = True }
    

-- getSessionId :: (MonadBase IO m) => Response LB.ByteString -> m (Maybe ByteString)
-- getSessionId res = do
--     ResponseMes {..} <- parseByteString (responseBody res)
--     return (encodeUtf8 . sessionIdText <$> sessionId)


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


mkRequest :: (ToJSON a) => Method -> ByteString -> a -> SessState Request
mkRequest meth path args = 
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

updateSessionId :: ResponseMes -> SessState ()
updateSessionId ResponseMes {..} = do
    s@Session {..} <- SessState get
    case (sessId , sessionId) of
        (_, Nothing) -> return ()
        (_, Just i) -> SessState (put (s {sessId = (Just . encodeUtf8 . sessionIdText) i}))


-- TODO : handle error status
parseResBodyStatus :: ResponseMes -> SessState (Maybe SomeException)
parseResBodyStatus ResponseMes {status = Just 0} = return Nothing
parseResBodyStatus _ = return Nothing

-- TODO : handle status other than 200
parseResponse :: Response LB.ByteString -> SessState (Either SomeException Value)
parseResponse res
    | status == 200 =
        if LB.null body
            then Right <$> (parseAesonResult . fromJSON $ Null)
            else do
                resMsg@ResponseMes{value} <- parseByteString body
                parseResBodyStatus resMsg >>= maybe
                    (updateSessionId resMsg >> return (Right value))
                    (return . Left . toException)
    | otherwise =  return . Left . toException . HTTPStatusUnknown status . statusMessage . responseStatus $ res
        where
            status = statusCode (responseStatus res)
            body = responseBody res


doCommand :: (ToJSON a) => Method -> ByteString -> a -> SessState Value
doCommand meth path args = mkRequest meth path args >>= sendRequest >>= parseResponse >>= either throwIO return
