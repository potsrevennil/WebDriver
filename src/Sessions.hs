{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Sessions where

import Data.ByteString (ByteString)
import Data.Aeson
import Data.Text
import Network.HTTP.Client (Manager)
import Control.Monad.Trans.State.Lazy
import Control.Monad.IO.Class
import Control.Monad.Base


newtype SessionId = SessionId {sessionIdText :: Text}
                  deriving (Eq, Ord, Show, Read, FromJSON, ToJSON)

data Session = Session {
        sessHost :: ByteString,
        sessPort :: Int,
        sessId :: Maybe ByteString,
        sessManager :: Manager
    } 

newtype SessState a = SessState {getSessState :: StateT Session IO a}
    deriving (Applicative, Functor, Monad, MonadIO, MonadBase IO)

