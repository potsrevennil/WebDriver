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
                >> findElement (XPath "//div[@id='searchform']/form[1]/div[2]/div[1]/div[1]/div[1]/div[2]/input[1]")
                >>= (\e -> 
                    elementClick e
                    >> elementSendKeys e "hello"
                    >> elementClear e
                )
                
                -- >>= flip elementSendKeys "hello"
                -- >>= flip getElementAttr "label"
                -- >>= flip getElementProperty "label"
                -- >>= getElementRect
            ))
        (Session {sessHost = "127.0.0.1", sessPort = 4444, sessId = SessionId "", sessManager = manager})
    print res



