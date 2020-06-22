{-# LANGUAGE OverloadedStrings, PartialTypeSignatures, FlexibleContexts #-}

import Network.HTTP.Client
import Control.Monad.Trans.State.Lazy
import Commands.Commands
import Sessions
import Data.LocationStrategy

main :: IO ()
main = do 
    -- bs <- B.readFile "colors.json"
    -- print $ bs^..values.key "color"._String

    manager <- newManager defaultManagerSettings 
    res <- evalStateT (getSessState (
                newSession >>
                navigateTo "http://google.com.tw" >> 
                findElements (CssSelector "div")
            ))
        (Session {sessHost = "127.0.0.1", sessPort = 4444, sessId = Nothing, sessManager = manager})
    print res



