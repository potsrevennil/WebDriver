{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Sessions where

import Data.ByteString.Lazy (ByteString)
import Data.Aeson
import Data.Text.Lazy
import Network.HTTP.Client (Manager)
import Control.Monad.Trans.State.Lazy
import Control.Monad.IO.Class
import Control.Monad.Base
import Control.Monad.Catch


newtype SessionId = SessionId {getSessionId :: Text}
                  deriving (Eq, Ord, Show, Read, FromJSON, ToJSON)

data Session = Session {
        sessHost :: ByteString,
        sessPort :: Int,
        sessId :: SessionId,
        sessManager :: Manager
    } 

newtype SessState a = SessState {getSessState :: StateT Session IO a}
    deriving (Applicative, Functor, Monad, MonadIO, MonadBase IO, MonadThrow)

