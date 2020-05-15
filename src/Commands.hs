{-# LANGUAGE OverloadedStrings, PartialTypeSignatures, DeriveGeneric, FlexibleContexts, NamedFieldPuns, RecordWildCards #-}
module Commands (
    getSessionId,
    parseByteString,
    mkRequest,
    sendRequest,
    newSession,
    delSession,
    navigateTo,
    getCurrentUrl,
    getStatus
)where

import Network.HTTP.Client
import Network.HTTP.Types
import Network.HTTP.Types.Header
import Data.Aeson
import Data.Aeson.Types
import Data.Attoparsec.ByteString.Lazy (Result(..))
import qualified Data.Attoparsec.ByteString.Lazy as AP
import qualified Data.ByteString.Lazy as LB
import Data.ByteString (ByteString, append)
import Data.Typeable
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Maybe (fromMaybe)
import Control.Monad.Base
import Control.Exception (Exception)
import Control.Exception.Lifted

import Control.Monad.Trans.State.Lazy
import GHC.Generics
import Capabilities
import Sessions

data ResponseMes = ResponseMes {
        sessionId :: Maybe SessionId,
        status :: Int,
        value :: Value
    } deriving (Eq, Show, Generic)

instance FromJSON ResponseMes where
    parseJSON = genericParseJSON defaultOptions
        {omitNothingFields = True }

getSessionId :: (MonadBase IO m) => Response LB.ByteString -> m (Maybe ByteString)
getSessionId res = do
    ResponseMes {..} <- parseByteString (responseBody res)
    return (encodeUtf8 . sessionIdText <$> sessionId)
        

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


mkRequest :: Method -> ByteString -> LB.ByteString -> SessState Request
mkRequest meth path args = return defaultRequest {
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
        path = "/wd/hub/" `append` path,
        method = meth,
        requestBody = RequestBodyLBS args
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

parseResBodyStatus :: (Exception e) => ResponseMes -> SessState (Maybe e)
parseResBodyStatus ResponseMes {status = 0} = return Nothing
parseResBodyStatus _ = return (Just (error "Error"))

parseResponse :: (FromJSON a, Exception e) => Response LB.ByteString -> SessState (Either e a)
parseResponse res 
    | status == 200 = 
        if LB.null body
            then Right <$> (parseAesonResult . fromJSON $ Null)
            else do
                resMsg@ResponseMes { value } <- parseByteString body
                parseResBodyStatus resMsg >>= maybe
                    (updateSessionId resMsg >> Right <$> parseAesonResult (fromJSON value))
                    (return . Left)
    | otherwise =  return . Left . error $ "Error"
        where 
            status = statusCode (responseStatus res) 
            body = responseBody res

doCommand :: Method -> ByteString -> LB.ByteString -> SessState (Response LB.ByteString)
doCommand meth path args = mkRequest meth path args >>= sendRequest

newSession :: SessState (Response LB.ByteString)
newSession = do
    res <- doCommand methodPost "session" $ (encode . toJSON . object) ["desiredCapabilities" .= defCapabilities]
    s@Session {..} <- SessState get
    si <- getSessionId res
    SessState (put (s {sessId = si}))
    return res

delSession :: SessState (Response LB.ByteString)
delSession = do
    Session { .. } <- SessState get
    doCommand methodDelete ("session/" `append` fromMaybe "" sessId) ""

navigateTo :: Text -> SessState (Response LB.ByteString)
navigateTo url = do
    Session { .. } <- SessState get
    doCommand methodPost ("session/" `append` fromMaybe "" sessId `append` "/url") $ (encode .toJSON .object) ["url" .= url]

getCurrentUrl :: SessState String
getCurrentUrl = do
    Session { .. } <- SessState get
    res <- doCommand methodGet ("session/" `append` fromMaybe "" sessId `append` "/url") ""
    ResponseMes { .. } <- parseByteString (responseBody res)
    case fromJSON value of
        Success val -> return val
        Error err -> throwIO $ BadJSON err

getStatus :: SessState (Response LB.ByteString)
getStatus = doCommand methodGet "status" ""

