{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Capabilities where

import Data.Aeson
import Data.Aeson.Types
import GHC.Generics
import Data.Default

data Capabilities = 
    Capabilities {
        browserName :: String,
        browserVersion :: Maybe String,
        platformName :: Maybe String,
        acceptInsecureCerts :: Maybe Bool,
        pageLoadStrategy :: Maybe String,
        proxy :: Maybe ProxyType,
        setWindowRect :: Maybe Bool,
        timeouts :: Maybe Timeouts,
        strictFileInteractability :: Maybe Bool,
        unhandledPromptBehavior :: Maybe String
    } deriving (Show, Generic)

        
instance ToJSON Capabilities where
    toJSON = genericToJSON defaultOptions
        { omitNothingFields = True }

instance FromJSON Capabilities where
    parseJSON = genericParseJSON defaultOptions
        {omitNothingFields = True }

instance Default Capabilities where
    def = Capabilities {
        browserName = "firefox",
        browserVersion = Nothing,
        platformName = Nothing,
        acceptInsecureCerts = Just False,
        pageLoadStrategy = Just "normal",
        proxy = Just def,
        setWindowRect = Nothing,
        timeouts = Just def,
        strictFileInteractability = Just False,
        unhandledPromptBehavior = Just "dismiss and notify"
    }

defCapabilities :: Capabilities
defCapabilities = def

data ProxyType = 
    ProxyType { 
            proxyType :: String,
            proxyAutoconfigUrl :: Maybe String,
            ftpProxy :: Maybe String,
            httpProxy :: Maybe String,
            noProxy :: Maybe [String],
            sslProxy :: Maybe String,       
            socksProxy :: Maybe String,
            socksVersion :: Maybe Int
          } deriving (Show, Generic)

instance ToJSON ProxyType where
    toJSON = genericToJSON defaultOptions
        { omitNothingFields = True }

instance FromJSON ProxyType where
    parseJSON = genericParseJSON defaultOptions
        { omitNothingFields = True }

instance Default ProxyType where
    def = ProxyType {
            proxyType = "SYSTEM",
            proxyAutoconfigUrl = Nothing,
            ftpProxy = Nothing,
            httpProxy = Nothing,
            noProxy = Nothing,
            sslProxy = Nothing,
            socksProxy = Nothing,
            socksVersion = Nothing
        }

data Timeouts = 
    Timeouts {
        script :: Maybe Int,
        pageLoad :: Maybe Int,
        implicit :: Maybe Int
    } deriving (Show, Generic)

instance ToJSON Timeouts where
    toJSON = genericToJSON defaultOptions
        { omitNothingFields = True }

instance FromJSON Timeouts where
    parseJSON = genericParseJSON defaultOptions
        { omitNothingFields = True }

instance Default Timeouts where
    def = Timeouts {
            script = Just 30000,
            pageLoad = Just 300000,
            implicit = Just 0
        }