{-# LANGUAGE OverloadedStrings, PartialTypeSignatures, FlexibleContexts #-}

import Network.HTTP.Client
import Control.Monad.Trans.State.Lazy
import Commands
import Sessions

main :: IO ()
main = do 
    manager <- newManager defaultManagerSettings 
    res <- evalStateT (getSessState (newSession >> navigateTo "http://google.com"))
        (Session {sessHost = "127.0.0.1", sessPort = 4444, sessId = Nothing, sessManager = manager})
    print res



