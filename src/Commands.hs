{-# LANGUAGE OverloadedStrings, PartialTypeSignatures, DeriveGeneric, FlexibleContexts, RecordWildCards #-}
module Commands (
    getSessionId,
    parseByteString,
    mkRequest,
    newSession,
    delSession,
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
import Data.Text.Encoding (encodeUtf8)
import Control.Monad.Base
import Control.Exception.Lifted
import Control.Monad.IO.Class
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

parseByteString :: (MonadBase IO m, FromJSON a) => LB.ByteString -> m a
parseByteString s = case AP.parse json s of
                Done _ res -> 
                    case fromJSON res of
                        Success val -> return val
                        Error err -> throwIO $ BadJSON err
                Fail _ _ err   -> throwIO $ BadJSON err


mkRequest :: Method -> ByteString -> RequestBody -> Request
mkRequest meth path args = defaultRequest {
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
        requestBody = args
    }

newSession :: SessState (Response LB.ByteString)
newSession = do
    s@Session {..} <- SessState get
    res <- liftBase (httpLbs req sessManager)
    si <- getSessionId res
    SessState (put (s {sessId = si}))
    return res
    where req = (mkRequest methodPost "session" . RequestBodyLBS . encode . toJSON . object) 
                    ["desiredCapabilities" .= defCapabilities]


delSession :: SessState (Response LB.ByteString)
delSession = do
    Session { .. } <- SessState get
    liftBase (httpLbs (req sessId) sessManager)
        where 
            req :: Maybe ByteString -> Request
            req (Just i) = (mkRequest methodDelete ("session/" `append` i) . RequestBodyLBS) ""
            req Nothing = (mkRequest methodDelete "session/" . RequestBodyLBS) ""

getStatus :: IO (Response LB.ByteString)
getStatus = newManager defaultManagerSettings >>= httpLbs req
    where req = (mkRequest methodGet "status" . RequestBodyLBS) ""
