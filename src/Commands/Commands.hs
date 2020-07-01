{-# LANGUAGE OverloadedStrings, PartialTypeSignatures, FlexibleContexts, NamedFieldPuns, RecordWildCards #-}
module Commands.Commands (
    newSession,
    delSession,
    navigateTo,
    getCurrentUrl,
    back,
    forward,
    refresh,
    getTitle,
    getStatus,
    getWindowHandle,
    closeWindow,
    switchToWindow,
    getWindowHandles,
    newWindow,
    switchToParentFrame,
    getWindowSize,
    setWindowSize,
    maximizeWindow,
    minimizeWindow,
    fullscreenWindow,
    findElement,
    findElements,
    findChildElement,
    findChildElements
)where


import qualified Network.HTTP.Types.URI as HTTP
import Network.HTTP.Types.Method


import qualified Data.HashMap.Lazy as HL (HashMap, lookup)
import Data.ByteString (ByteString, append)

import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Aeson.Types
import Data.Aeson
import Data.Word
import Control.Exception.Lifted (throwIO)
import Control.Monad.Trans.State.Lazy

import Capabilities
import Sessions
import Commands.Internal
import Data.LocationStrategy

import Control.Lens hiding ((.=))
import Data.Aeson.Lens

----------------------------------------------------------------
--                                                            --
--                      Navigate Commands                     --
--                                                            --
----------------------------------------------------------------

newSession :: SessState Session
newSession = do
    ignore $ doCommand "session" methodPost $ object ["desiredCapabilities" .= defCapabilities]
    SessState get

delSession :: SessState ()
delSession = ignore $ doSessCommand "" methodDelete Null

navigateTo :: Text -> SessState ()
navigateTo url = ignore $ doSessCommand "/url" methodPost $ object ["url" .= url]

getCurrentUrl :: SessState Text
getCurrentUrl = doSessCommand "/url" methodGet Null

back :: SessState ()
back = ignore $ doSessCommand "/back" methodPost Null

forward :: SessState ()
forward = ignore $ doSessCommand "/forward" methodPost Null

refresh :: SessState ()
refresh = ignore $ doSessCommand "/forward" methodPost Null
    
getTitle :: SessState Text
getTitle = doSessCommand "/title" methodGet Null
    
getStatus :: SessState Value
getStatus = do
    res <- retValue $ doCommand "status" methodGet Null
    case (\r m -> object ["ready" .= r, "message" .= m]) <$>
            (res ^? key "ready" . _Bool) <*> 
            (res ^? key "message" . _String) of
        Just d -> return d
        Nothing -> throwIO $ BadJSON "Cannot parse result of getStatus command into (ready, message) pair."

----------------------------------------------------------------
--                                                            --
--                      Window Commands                       --
--                                                            --
----------------------------------------------------------------

getWindowHandle :: SessState Text
getWindowHandle = doSessWinCommand "" methodGet Null   

closeWindow :: SessState ()
closeWindow = ignore $ doSessWinCommand "" methodDelete Null

switchToWindow :: Text -> SessState ()
switchToWindow handle = ignore $ doSessWinCommand "" methodPost $ object ["handle" .= handle]


getWindowHandles :: SessState [Text]
getWindowHandles = doSessCommand "/window_handles" methodGet Null
    
newWindow :: Text -> SessState Value
newWindow t = do
    res <- retValue $ doSessWinCommand "new" methodPost $ object ["type" .= t]
    case (\h t -> object ["handle" .= h, "type" .= t]) <$>
        (res ^? key "handle" . _String) <*>
        (res ^? key "type" . _String) of
            Just d -> return d
            Nothing -> throwIO $ BadJSON "Cannot parse result of newWindow command into (handle, type) pair."
-- TODO :
-- switchToFrame :: WebFrame -> SessState ()
-- switchToFrame = do

switchToParentFrame :: SessState ()
switchToParentFrame = ignore $ doSessCommand "/frame/parent" methodPost Null

getWindowSize :: Text -> SessState Value
getWindowSize handle = do
    res <- retValue $ doSessWinCommand (encodeUtf8 handle `append` "/size") methodGet Null

    case (\w h x y -> object ["width" .= w, "height" .= h, "x" .= x, "y" .= y]) <$>
            (res ^? key "width" . _Number) <*> 
            (res ^? key "height" . _Number) <*>
            (res ^? key "x" . _Number) <*>
            (res ^? key "y" . _Number) of
        Just d -> return d
        Nothing -> throwIO $ BadJSON "Cannot parse result of getWindowSize command into (height, width, x, y) tuple."
        
setWindowSize :: Text -> Maybe Word32 -> Maybe Word32 -> Maybe Word -> Maybe Word -> SessState Value
setWindowSize handle w h x y = doSessWinCommand (encodeUtf8 handle `append` "/size") methodPost 
                                    $ object ["width" .= w, "height" .= h, "x" .= x, "y" .= y]

maximizeWindow :: Text -> SessState Value
maximizeWindow handle = doSessWinCommand (encodeUtf8 handle `append` "/maximize" ) methodPost Null

minimizeWindow :: Text -> SessState Value
minimizeWindow handle = doSessWinCommand (encodeUtf8 handle `append` "/minimize" ) methodPost Null
    
fullscreenWindow :: Text -> SessState Value
fullscreenWindow handle = doSessWinCommand "fullscreen" methodPost Null
    
----------------------------------------------------------------
--                                                            --
--                      Element Commands                      --
--                                                            --
----------------------------------------------------------------


findElement :: LocationStrategy -> SessState Value
findElement = doSessCommand "/element" methodPost


findElements :: LocationStrategy -> SessState [Value]
findElements = doSessCommand "/elements" methodPost

findChildElement :: Text -> LocationStrategy -> SessState Value
findChildElement eid = doSessCommand ("/element/" `append` encodeUtf8 eid `append` "/element") methodPost


findChildElements :: Text -> LocationStrategy -> SessState [Value]
findChildElements eid = doSessCommand ("/element/" `append` encodeUtf8 eid `append` "/elements") methodPost