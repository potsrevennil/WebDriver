{-# LANGUAGE OverloadedStrings, PartialTypeSignatures, FlexibleContexts, NamedFieldPuns, RecordWildCards #-}
module Commands.Commands (
    getSessionId,
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
    fullscreenWindow
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

import Capabilities
import Sessions
import Commands.Internal




----------------------------------------------------------------
--                                                            --
--                      Navigate Commands                     --
--                                                            --
----------------------------------------------------------------

newSession :: SessState Session
newSession = do
    doCommand methodPost "session" $ (encode . toJSON . object) ["desiredCapabilities" .= defCapabilities] :: SessState ResponseMes
    SessState get

delSession :: SessState ()
delSession = do
    Session { .. } <- SessState get
    doCommand methodDelete ("session/" `append` fromMaybe "" sessId) "" :: SessState ResponseMes
    return ()

navigateTo :: Text -> SessState ()
navigateTo url = do
    Session { .. } <- SessState get
    ResponseMes { value } <- doCommand methodPost ("session/" `append` fromMaybe "" sessId `append` "/url") $ (encode .toJSON .object) ["url" .= url]
    return ()

getCurrentUrl :: SessState Text
getCurrentUrl = do
    Session { .. } <- SessState get
    ResponseMes { value } <- doCommand methodGet ("session/" `append` fromMaybe "" sessId `append` "/url") ""
    parseAesonResult (fromJSON value)

back :: SessState ()
back = do
    Session { .. } <- SessState get
    ResponseMes { value } <- doCommand methodPost ("session/" `append` fromMaybe "" sessId `append` "/back") ""
    return ()

forward :: SessState ()
forward = do
    Session { .. } <- SessState get
    ResponseMes { value } <- doCommand methodPost ("session/" `append` fromMaybe "" sessId `append` "/forward") ""
    return ()

refresh :: SessState ()
refresh = do
    Session { .. } <- SessState get
    ResponseMes { value } <- doCommand methodPost ("session/" `append` fromMaybe "" sessId `append` "/forward") ""
    return ()

getTitle :: SessState Text
getTitle = do
    Session { .. } <- SessState get
    ResponseMes { value } <- doCommand methodGet ("session/" `append` fromMaybe "" sessId `append` "/title") ""
    parseAesonResult (fromJSON value)

getStatus :: SessState Value
getStatus = do
    ResponseMes { value } <- doCommand methodGet "status" ""
    return value

----------------------------------------------------------------
--                                                            --
--                      Window Commands                       --
--                                                            --
----------------------------------------------------------------

getWindowHandle :: SessState Text
getWindowHandle = do
    Session { .. } <- SessState get
    ResponseMes { value } <- doCommand methodGet ("session/" `append` fromMaybe "" sessId `append` "/window") ""
    parseAesonResult (fromJSON value)

closeWindow :: SessState ()
closeWindow = do
    Session { .. } <- SessState get
    ResponseMes { value } <- doCommand methodDelete ("session/" `append` fromMaybe "" sessId `append` "/window") ""
    return ()

switchToWindow :: Text -> SessState ()
switchToWindow handle = do
    Session { .. } <- SessState get
    _ <- doCommand methodPost ("session/" `append` fromMaybe "" sessId `append` "/window") $ (encode .toJSON .object) ["handle" .= handle]
    return ()


getWindowHandles :: SessState [Text]
getWindowHandles = do
    Session { .. } <- SessState get
    ResponseMes { value } <- doCommand methodGet ("session/" `append` fromMaybe "" sessId `append` "/window_handles") ""
    parseAesonResult (fromJSON value)

newWindow :: Text -> SessState (HL.HashMap Text Value)
newWindow t = do
    Session { .. } <- SessState get
    ResponseMes { value } <- doCommand methodPost ("session/" `append` fromMaybe "" sessId `append` "/window/new") $ (encode .toJSON .object) ["type" .= t]
    parseAesonResult (fromJSON value)

-- TODO :
-- switchToFrame :: WebFrame -> SessState ()
-- switchToFrame = do

switchToParentFrame :: SessState ()
switchToParentFrame = do
    Session { .. } <- SessState get
    doCommand methodPost ("session/" `append` fromMaybe "" sessId `append` "/frame/parent") ""
    return ()

getWindowSize :: Text -> SessState Value
getWindowSize handle = do
    Session { .. } <- SessState get
    ResponseMes {value} <- doCommand methodGet ("session/" `append` fromMaybe "" sessId `append` "/window/" `append` encodeUtf8 handle `append` "/size") ""

    case value of
        Object o ->
            (\h w -> object ["width" .= w, "height" .= h]) <$>
                (parseAesonResult $ parse (.: "width") o :: SessState Value) <*>
                (parseAesonResult $ parse (.: "height") o :: SessState Value)
        _ -> throwIO $ BadJSON "Cannot parse non-object JSON as a (height, width) pair"

setWindowSize :: Text -> Maybe Word32 -> Maybe Word32 -> Maybe Word -> Maybe Word -> SessState Value
setWindowSize handle w h x y = do
    Session { .. } <- SessState get
    ResponseMes {value} <- doCommand methodPost ("session/" `append` fromMaybe "" sessId `append` "/window/" `append` encodeUtf8 handle `append` "/size")
        $ (encode . toJSON . object) ["width" .= w, "height" .= h, "x" .= x, "y" .= y]
    return value

maximizeWindow :: Text -> SessState Value
maximizeWindow handle = do
    Session { .. } <- SessState get
    ResponseMes {value} <- doCommand methodPost ("session/" `append` fromMaybe "" sessId `append` "/window/" `append` encodeUtf8 handle `append` "/maximize" ) ""
    return value


minimizeWindow :: Text -> SessState Value
minimizeWindow handle = do
    Session { .. } <- SessState get
    ResponseMes {value} <- doCommand methodPost ("session/" `append` fromMaybe "" sessId `append` "/window/" `append` encodeUtf8 handle `append` "/minimize" ) ""
    return value

fullscreenWindow :: Text -> SessState Value
fullscreenWindow handle = do
    Session { .. } <- SessState get
    ResponseMes {value} <- doCommand methodPost ("session/" `append` fromMaybe "" sessId `append` "/window/fullscreen" ) ""
    return value
