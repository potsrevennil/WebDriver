{-# LANGUAGE OverloadedStrings, PartialTypeSignatures, FlexibleContexts, RecordWildCards #-}

import Commands.Commands
import Sessions
import Data.LocationStrategy
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T

import qualified Data.ByteString.Lazy as B
import qualified Data.Text.Lazy.Encoding as T
import Control.Monad.Base
import Data.Aeson
import Test
import Commands.Internal
import Network.HTTP.Types.Method
import Network.HTTP.Client
import Network.HTTP.Client.TLS

main :: IO ()
main = do
    res <- runSessionTls (do
                    newSession 
                    navigateTo "https://google.com"
            )
    print res
    




