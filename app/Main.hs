{-# LANGUAGE OverloadedStrings, PartialTypeSignatures, FlexibleContexts #-}
module Main where

import Network.HTTP.Client
import Commands
import Sessions

main :: IO ()
main = do 
    res <- newSession
    mResId <- getSessionId res
    case mResId of
        Just i -> do
            res' <- delSession i
            print res'
        Nothing -> print "Delete Session Failed."


