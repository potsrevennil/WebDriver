{-# LANGUAGE OverloadedStrings, PartialTypeSignatures, RecordWildCards, FlexibleContexts #-}
module Main where

import Network.HTTP.Client
import qualified Data.ByteString.UTF8 as BSU
import Commands

main :: IO ()
main = do 
    res <- newSession
    ResponseMes {..} <- parseByteString (responseBody res)
    case sessionId of
        Just id -> do
            res' <- delSession (BSU.fromString id)
            print res'
        Nothing -> print "Failed to get sessionId"


