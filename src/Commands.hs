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
    getStatus,
    getWindowHandle,
    closeWindow,
    switchToWindow,
    newWindow,
    getWindowHandles
)where

import Network.HTTP.Client
import Network.HTTP.Types
import Network.HTTP.Types.Header
import Data.Aeson
import Data.Aeson.Types
import Data.Attoparsec.ByteString.Lazy (Result(..))
import qualified Data.Attoparsec.ByteString.Lazy as AP
import qualified Data.ByteString.Lazy as LB
import qualified Data.HashMap.Lazy as HL (HashMap, lookup)
import Data.ByteString (ByteString, append)
import Data.Typeable
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Maybe (fromMaybe)
import Control.Monad.Base
import Control.Exception (Exception, SomeException(..), toException)
import Control.Exception.Lifted (throwIO)

import Control.Monad.Trans.State.Lazy
import GHC.Generics
import Capabilities
import Sessions

data ResponseMes = ResponseMes {
        sessionId :: Maybe SessionId,
        status :: Maybe Int,
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
        path = path,
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


-- TODO : handle error status
parseResBodyStatus :: ResponseMes -> SessState (Maybe SomeException)
parseResBodyStatus ResponseMes {status = Just 0} = return Nothing
parseResBodyStatus _ = return Nothing

-- TODO : handle status other than 200
parseResponse :: Response LB.ByteString -> SessState (Either SomeException ResponseMes)
parseResponse res 
    | status == 200 = 
        if LB.null body
            then Right <$> (parseAesonResult . fromJSON $ Null)
            else do
                resMsg <- parseByteString body
                parseResBodyStatus resMsg >>= maybe
                    (updateSessionId resMsg >> return (Right resMsg))
                    (return . Left . toException)
    | otherwise =  return . Left . toException . HTTPStatusUnknown status . statusMessage . responseStatus $ res
        where 
            status = statusCode (responseStatus res) 
            body = responseBody res

doCommand :: Method -> ByteString -> LB.ByteString -> SessState ResponseMes
doCommand meth path args = mkRequest meth path args >>= sendRequest >>= parseResponse >>= either throwIO return

newSession :: SessState Session
newSession = do
    doCommand methodPost "session" $ (encode . toJSON . object) ["desiredCapabilities" .= defCapabilities] :: SessState ResponseMes
    SessState get

delSession :: SessState ()
delSession = do
    Session { .. } <- SessState get
    doCommand methodDelete ("session/" `append` fromMaybe "" sessId) "" :: SessState ResponseMes
    return ()

navigateTo :: Text -> SessState ()
navigateTo url = do
    Session { .. } <- SessState get
    ResponseMes { value } <- doCommand methodPost ("session/" `append` fromMaybe "" sessId `append` "/url") $ (encode .toJSON .object) ["url" .= url]
    return ()

getCurrentUrl :: SessState Text
getCurrentUrl = do
    Session { .. } <- SessState get
    ResponseMes { value } <- doCommand methodGet ("session/" `append` fromMaybe "" sessId `append` "/url") ""
    parseAesonResult (fromJSON value)

getStatus :: SessState Value
getStatus = do
    ResponseMes { value } <- doCommand methodGet "status" ""
    return value 

getWindowHandle :: SessState Text
getWindowHandle = do
    Session { .. } <- SessState get
    ResponseMes { value } <- doCommand methodGet ("session/" `append` fromMaybe "" sessId `append` "/window") ""
    parseAesonResult (fromJSON value)

closeWindow :: SessState ()
closeWindow = do
    Session { .. } <- SessState get
    ResponseMes { value } <- doCommand methodDelete ("session/" `append` fromMaybe "" sessId `append` "/window") ""
    return ()

switchToWindow :: Text -> SessState ()
switchToWindow handle = do
    Session { .. } <- SessState get
    _ <- doCommand methodPost ("session/" `append` fromMaybe "" sessId `append` "/window") $ (encode .toJSON .object) ["handle" .= handle]
    return ()

newWindow :: Text -> SessState (HL.HashMap Text Value)
newWindow t = do
    Session { .. } <- SessState get
    ResponseMes { value } <- doCommand methodPost ("session/" `append` fromMaybe "" sessId `append` "/window/new") $ (encode .toJSON .object) ["type" .= t]
    parseAesonResult (fromJSON value)

getWindowHandles :: SessState [Text]
getWindowHandles = do
    Session { .. } <- SessState get
    ResponseMes { value } <- doCommand methodGet ("session/" `append` fromMaybe "" sessId `append` "/window_handles") ""
    parseAesonResult (fromJSON value)
