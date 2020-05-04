{-# LANGUAGE OverloadedStrings, PartialTypeSignatures #-}
module Main where

import Network.HTTP.Client
import Network.HTTP.Types
import Network.HTTP.Types.Header
import Data.Aeson
import Data.Aeson.Types
import Data.Attoparsec.ByteString.Lazy (Result(..))
import qualified Data.Attoparsec.ByteString.Lazy as AP
import Data.HashMap.Strict ((!))
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.UTF8 as BSU
import Data.ByteString (ByteString, append)
import Capabilities
-- import GHC.Generics

getSessionId :: Response LB.ByteString -> String
getSessionId res = case AP.parse json (responseBody res) of
                        Done _ r -> 
                            case r of 
                                Object hm -> 
                                    case hm ! "sessionId" of
                                        String s -> filter (\x -> x /= '\"') (show s)
                                        _ -> ""
                                _ -> ""
                        _ -> ""


newSession :: Request
newSession = defaultRequest {
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
        path = "/wd/hub/session",
        method = methodPost,
        requestBody = RequestBodyLBS 
            (encode $ toJSON $ object ["desiredCapabilities" .= defCapabilities])
    }

delSession :: Response LB.ByteString -> Request
delSession res = defaultRequest {
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
        path = "/wd/hub/session/" `append` BSU.fromString (getSessionId res),
        method = methodDelete
    } 

main :: IO ()
main = do 
    manager <- newManager defaultManagerSettings
    res <- httpLbs newSession manager
    print ("/wd/hub/" `append` BSU.fromString (getSessionId res))
    res' <- httpLbs (delSession res) manager
    print res'

