{-# LANGUAGE OverloadedStrings, PartialTypeSignatures, FlexibleContexts, NamedFieldPuns, RecordWildCards #-}
module Commands.Commands (
    parseByteString,
    mkRequest,
    sendRequest,
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
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Maybe (fromMaybe)
import Data.Aeson.Types
import Data.Aeson
import Data.Word
import Control.Exception.Lifted (throwIO)
import Control.Monad.Trans.State.Lazy
import Control.Monad (void)

import Capabilities
import Sessions
import Commands.Internal
import Data.LocationStrategy

----------------------------------------------------------------
--                                                            --
--                      Navigate Commands                     --
--                                                            --
----------------------------------------------------------------

newSession :: SessState Session
newSession = do
    doCommand methodPost "session" $ object ["desiredCapabilities" .= defCapabilities]
    SessState get

delSession :: SessState ()
delSession = do
    Session { .. } <- SessState get
    void $ doCommand methodDelete ("session/" `append` fromMaybe "" sessId) Null

navigateTo :: Text -> SessState ()
navigateTo url = do
    Session { .. } <- SessState get
    void $ doCommand methodPost ("session/" `append` fromMaybe "" sessId `append` "/url") $ object ["url" .= url]

getCurrentUrl :: SessState Text
getCurrentUrl = do
    Session { .. } <- SessState get
    v <- doCommand methodGet ("session/" `append` fromMaybe "" sessId `append` "/url") Null
    parseAesonResult (fromJSON v)

back :: SessState ()
back = do
    Session { .. } <- SessState get
    void $ doCommand methodPost ("session/" `append` fromMaybe "" sessId `append` "/back") Null

forward :: SessState ()
forward = do
    Session { .. } <- SessState get
    void $ doCommand methodPost ("session/" `append` fromMaybe "" sessId `append` "/forward") Null

refresh :: SessState ()
refresh = do
    Session { .. } <- SessState get
    void $ doCommand methodPost ("session/" `append` fromMaybe "" sessId `append` "/forward") Null

getTitle :: SessState Text
getTitle = do
    Session { .. } <- SessState get
    v <- doCommand methodGet ("session/" `append` fromMaybe "" sessId `append` "/title") Null
    parseAesonResult (fromJSON v)

getStatus :: SessState Value
getStatus = doCommand methodGet "status" Null

----------------------------------------------------------------
--                                                            --
--                      Window Commands                       --
--                                                            --
----------------------------------------------------------------

getWindowHandle :: SessState Text
getWindowHandle = do
    Session { .. } <- SessState get
    v <- doCommand methodGet ("session/" `append` fromMaybe "" sessId `append` "/window") Null
    parseAesonResult (fromJSON v)

closeWindow :: SessState ()
closeWindow = do
    Session { .. } <- SessState get
    void $ doCommand methodDelete ("session/" `append` fromMaybe "" sessId `append` "/window") Null

switchToWindow :: Text -> SessState ()
switchToWindow handle = do
    Session { .. } <- SessState get
    void $ doCommand methodPost ("session/" `append` fromMaybe "" sessId `append` "/window") $ object ["handle" .= handle]


getWindowHandles :: SessState [Text]
getWindowHandles = do
    Session { .. } <- SessState get
    v <- doCommand methodGet ("session/" `append` fromMaybe "" sessId `append` "/window_handles") Null
    parseAesonResult (fromJSON v)

newWindow :: Text -> SessState (HL.HashMap Text Value)
newWindow t = do
    Session { .. } <- SessState get
    v <- doCommand methodPost ("session/" `append` fromMaybe "" sessId `append` "/window/new") $ object ["type" .= t]
    parseAesonResult (fromJSON v)

-- TODO :
-- switchToFrame :: WebFrame -> SessState ()
-- switchToFrame = do

switchToParentFrame :: SessState ()
switchToParentFrame = do
    Session { .. } <- SessState get
    void $ doCommand methodPost ("session/" `append` fromMaybe "" sessId `append` "/frame/parent") Null

getWindowSize :: Text -> SessState Value
getWindowSize handle = do
    Session { .. } <- SessState get
    v <- doCommand methodGet ("session/" `append` fromMaybe "" sessId `append` "/window/" `append` encodeUtf8 handle `append` "/size") Null

    case v of
        Object o ->
            (\h w x y -> object ["width" .= w, "height" .= h, "x" .= x, "y" .= y]) <$>
                (parseAesonResult $ parse (.: "width") o :: SessState Value) <*>
                (parseAesonResult $ parse (.: "height") o :: SessState Value) <*>
                (parseAesonResult $ parse (.: "x") o :: SessState Value) <*>
                (parseAesonResult $ parse (.: "y") o :: SessState Value) 
        _ -> throwIO $ BadJSON "Cannot parse non-object JSON as a (height, width) pair"
        
setWindowSize :: Text -> Maybe Word32 -> Maybe Word32 -> Maybe Word -> Maybe Word -> SessState Value
setWindowSize handle w h x y = do
    Session { .. } <- SessState get
    doCommand methodPost ("session/" `append` fromMaybe "" sessId `append` "/window/" `append` encodeUtf8 handle `append` "/size")
        $ object ["width" .= w, "height" .= h, "x" .= x, "y" .= y]

maximizeWindow :: Text -> SessState Value
maximizeWindow handle = do
    Session { .. } <- SessState get
    doCommand methodPost ("session/" `append` fromMaybe "" sessId `append` "/window/" `append` encodeUtf8 handle `append` "/maximize" ) Null

minimizeWindow :: Text -> SessState Value
minimizeWindow handle = do
    Session { .. } <- SessState get
    doCommand methodPost ("session/" `append` fromMaybe "" sessId `append` "/window/" `append` encodeUtf8 handle `append` "/minimize" ) Null
    
fullscreenWindow :: Text -> SessState Value
fullscreenWindow handle = do
    Session { .. } <- SessState get
    doCommand methodPost ("session/" `append` fromMaybe "" sessId `append` "/window/fullscreen" ) Null
    
----------------------------------------------------------------
--                                                            --
--                      Element Commands                      --
--                                                            --
----------------------------------------------------------------


findElement :: LocationStrategy -> SessState Value
findElement ls = do
    Session { .. } <- SessState get
    doCommand methodPost ("session/" `append` fromMaybe "" sessId `append` "/element" ) ls


findElements :: LocationStrategy -> SessState [Value]
findElements ls = do
    Session { .. } <- SessState get
    v <- doCommand methodPost ("session/" `append` fromMaybe "" sessId `append` "/elements" ) ls
    parseAesonResult (fromJSON v)

findChildElement :: Text -> LocationStrategy -> SessState Value
findChildElement eid ls = do
    Session { .. } <- SessState get
    doCommand methodPost ("session/" `append` fromMaybe "" sessId `append` "/element/" `append` encodeUtf8 eid `append` "/element") ls


findChildElements :: Text -> LocationStrategy -> SessState [Value]
findChildElements eid ls = do
    Session { .. } <- SessState get
    v <- doCommand methodPost ("session/" `append` fromMaybe "" sessId `append` "/element/" `append` encodeUtf8 eid `append` "/elements" ) ls
    parseAesonResult (fromJSON v)