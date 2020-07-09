module Exceptions where

import Data.ByteString
import Data.Typeable
import Control.Exception (Exception)

instance Exception BadJSON
newtype BadJSON = BadJSON String
             deriving (Eq, Show, Typeable)

data HTTPStatusUnknown = HTTPStatusUnknown Int ByteString deriving (Eq, Show, Typeable)
instance Exception HTTPStatusUnknown