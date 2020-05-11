{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Sessions where

import Data.ByteString (ByteString)
import Data.Aeson
import Data.Text
import Network.HTTP.Client (Manager)


newtype SessionId = SessionId {sessionIdText :: Text}
                  deriving (Eq, Ord, Show, Read, FromJSON, ToJSON)

data Session = Session {
        sessHost :: ByteString,
        sessPort :: Int,
        sessId :: Maybe String,
        sessManager :: Manager
    } 