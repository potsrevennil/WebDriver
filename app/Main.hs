{-# LANGUAGE OverloadedStrings, PartialTypeSignatures, FlexibleContexts #-}

import Network.HTTP.Client
import Control.Monad.Trans.State.Lazy
import Commands.Commands
import Sessions
import Data.LocationStrategy

main :: IO ()
main = do 
    manager <- newManager defaultManagerSettings 
    res <- evalStateT (getSessState (
                newSession 
                >> navigateTo "http://google.com"
                >> getWindowSize "current"
            ))
        (Session {sessHost = "127.0.0.1", sessPort = 4444, sessId = SessionId "", sessManager = manager})
    print res



