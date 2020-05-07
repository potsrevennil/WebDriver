{-# LANGUAGE OverloadedStrings, PartialTypeSignatures, DeriveGeneric, FlexibleContexts #-}
module Commands where

import Network.HTTP.Client
import Network.HTTP.Types
import Network.HTTP.Types.Header
import Data.Aeson
import Data.Aeson.Types
import Data.Attoparsec.ByteString.Lazy (Result(..))
import qualified Data.Attoparsec.ByteString.Lazy as AP
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.UTF8 as BSU
import Data.ByteString (ByteString, append)
import Data.Typeable
import Control.Monad.Base
import Control.Exception.Lifted
import GHC.Generics
import Capabilities

data ResponseMes = ResponseMes {
        sessionId :: Maybe String,
        status :: Int,
        value :: Value
    } deriving (Eq, Show, Generic)

instance FromJSON ResponseMes where
    parseJSON = genericParseJSON defaultOptions
        {omitNothingFields = True }

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

newSession :: IO (Response LB.ByteString)
newSession = newManager defaultManagerSettings >>= httpLbs req
    where req = (mkRequest methodPost "session" . RequestBodyLBS . encode . toJSON . object) 
                    ["desiredCapabilities" .= defCapabilities]

delSession :: ByteString -> IO (Response LB.ByteString)
delSession sessionId = do
    manager <- newManager defaultManagerSettings
    httpLbs req manager
    where req = (mkRequest methodDelete ("session/" `append` sessionId) . RequestBodyLBS) ""

getStatus' :: IO (Response LB.ByteString)
getStatus' = newManager defaultManagerSettings >>= httpLbs req
    where req = (mkRequest methodGet "status" . RequestBodyLBS) ""
