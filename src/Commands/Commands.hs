{-# LANGUAGE OverloadedStrings, PartialTypeSignatures, FlexibleContexts, NamedFieldPuns, RecordWildCards #-}
module Commands.Commands 
(
    runSession
    , runSessionTls
    , newSession
    , delSession
    , navigateTo
    , getCurrentUrl
    , back
    , forward
    , refresh
    , getTitle
    , getStatus
    , getWindowHandle
    , closeWindow
    , switchToWindow
    , getWindowHandles
    , newWindow
    , switchToFrame
    , switchToParentFrame
    , getWindowSize
    , setWindowSize
    , maximizeWindow
    , minimizeWindow
    , fullscreenWindow
    , findElement
    , findElements
    , findChildElement
    , findChildrenElement
    , getActiveElement
    , isElementSelected
    , getElementAttr
    , getElementProperty
    , getElementCssValue
    , getElementText
    , getElementTagName
    , getElementRect
    , isElementEnabled
    , getComputedRole
    , getComputedLabel
    , elementClick
    , elementClear
    , elementSendKeys
    , executeScript

    , elementScreenshot
    , elementSaveScreenshot


    , acceptAlert
    , getAlert
)
where


import qualified Network.HTTP.Types.URI as HTTP
import Network.HTTP.Types.Method
import Network.HTTP.Client
import Network.HTTP.Client.TLS

import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HL 
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Base64.Lazy as B

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T
import Data.Aeson.Types
import Data.Aeson
import Data.Word
import Data.Traversable
import Control.Exception.Lifted (throwIO)
import Control.Monad.Trans.State.Lazy
import Control.Monad.Base

import Capabilities
import Sessions
import Commands.Internal
import Data.LocationStrategy
import Exceptions

import Control.Lens hiding ((.=))
import Data.Aeson.Lens

----------------------------------------------------------------
--                                                            --
--                      Navigate Commands                     --
--                                                            --
----------------------------------------------------------------

runSession :: SessState a -> IO a
runSession f = do
    manager <- newManager defaultManagerSettings
    evalStateT (getSessState f) (Session {sessHost = "127.0.0.1", sessPort = 4444, sessId = SessionId "", sessManager = manager})

runSessionTls :: SessState a -> IO a
runSessionTls f = do
    manager <- newTlsManager 
    evalStateT (getSessState f) (Session {sessHost = "127.0.0.1", sessPort = 4444, sessId = SessionId "", sessManager = manager})

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
switchToFrame :: Frame -> SessState ()
switchToFrame f = 
    ignore $ doSessCommand "/frame" methodPost $ object ["id" .= f]

switchToParentFrame :: SessState ()
switchToParentFrame = ignore $ doSessCommand "/frame/parent" methodPost Null

getWindowSize :: Text -> SessState Value
getWindowSize handle = do
    res <- retValue $ doSessWinCommand (T.encodeUtf8 handle `B.append` "/size") methodGet Null

    case (\w h x y -> object ["width" .= w, "height" .= h, "x" .= x, "y" .= y]) <$>
            (res ^? key "width" . _Number) <*>
            (res ^? key "height" . _Number) <*>
            (res ^? key "x" . _Number) <*>
            (res ^? key "y" . _Number) of
        Just d -> return d
        Nothing -> throwIO $ BadJSON "Cannot parse result of getWindowSize command into (height, width, x, y) tuple."

setWindowSize :: Text -> Maybe Word32 -> Maybe Word32 -> Maybe Word -> Maybe Word -> SessState Value
setWindowSize handle w h x y = doSessWinCommand (T.encodeUtf8 handle `B.append` "/size") methodPost
                                    $ object ["width" .= w, "height" .= h, "x" .= x, "y" .= y]

maximizeWindow :: Text -> SessState Value
maximizeWindow handle = doSessWinCommand (T.encodeUtf8 handle `B.append` "/maximize" ) methodPost Null

minimizeWindow :: Text -> SessState Value
minimizeWindow handle = doSessWinCommand (T.encodeUtf8 handle `B.append` "/minimize" ) methodPost Null

fullscreenWindow :: Text -> SessState Value
fullscreenWindow handle = doSessWinCommand "fullscreen" methodPost Null

----------------------------------------------------------------
--                                                            --
--                      Element Commands                      --
--                                                            --
----------------------------------------------------------------


findElement :: LocationStrategy -> SessState Text
findElement ls = do
    res <- retValue $ doSessCommand "/element" methodPost ls
    case preview _Object res of
        Just d -> (parseAesonResult . fromJSON) $ head (HL.elems d)
        Nothing -> throwIO $ BadJSON "Cannot find the element."

findElements :: LocationStrategy -> SessState [Text]
findElements ls = do
    res <- retValue $ doSessCommand "/elements" methodPost ls
    case preview _Object res of
        Just d -> mapM (parseAesonResult . fromJSON) (HL.elems d)
        Nothing -> throwIO $ BadJSON "Cannot find the elements."

findChildElement :: LocationStrategy -> Text -> SessState Text
findChildElement ls eid = do
    res <- retValue $ doSessElCommand eid "/element" methodPost ls
    case preview _Object res of
        Just d -> parseAesonResult . fromJSON . head . HL.elems $ d
        Nothing -> throwIO $ BadJSON "Cannot find child of the element."


findChildrenElement :: LocationStrategy -> Text -> SessState [Text]
findChildrenElement ls eid = do 
    res <- retValue $ doSessElCommand eid "/elements" methodPost ls
    case preview _Object res of
        Just d -> mapM (parseAesonResult  . fromJSON) (HL.elems d)
        Nothing -> throwIO $ BadJSON "Cannot find children of the element."

getActiveElement :: SessState Text
getActiveElement = do
    res <- retValue $ doSessCommand "/element/active" methodPost Null
    case preview _Object res of
        Just e -> parseAesonResult . fromJSON .head . HL.elems $ e
        Nothing -> throwIO $ BadJSON "There is no active element."

isElementSelected :: Text -> SessState Bool
isElementSelected eid = 
    doSessElCommand eid "/selected" methodGet Null

getElementAttr :: Text -> Text -> SessState Value
getElementAttr name eid = 
    doSessElCommand eid ("/attribute/" `B.append` T.encodeUtf8 name) methodGet $ object ["name" .= name]

getElementProperty ::  Text -> Text -> SessState Value
getElementProperty name eid =
    doSessElCommand eid ("/property/" `B.append` T.encodeUtf8 name) methodGet $ object ["name" .= name]

getElementCssValue :: Text -> Text -> SessState Value
getElementCssValue cssProp eid = 
    doSessElCommand eid ("/css/" `B.append` T.encodeUtf8 cssProp) methodGet $ object ["propertyName" .= cssProp]

getElementText :: Text -> SessState Text
getElementText eid = 
    doSessElCommand eid "/text" methodGet Null

getElementTagName :: Text -> SessState Text
getElementTagName eid =
    doSessElCommand eid "/name" methodGet Null

getElementRect :: Text -> SessState Value
getElementRect eid =
    doSessElCommand eid "/rect" methodGet Null

isElementEnabled :: Text -> SessState Bool
isElementEnabled eid =
    doSessElCommand eid "/enabled" methodGet Null

getComputedRole :: Text -> SessState Value
getComputedRole eid =
    doSessElCommand eid "/computedrole" methodGet Null

getComputedLabel :: Text -> SessState Value
getComputedLabel eid =
    doSessElCommand eid "computedlabel" methodGet Null

elementClick :: Text -> SessState ()
elementClick eid =
    ignore $ doSessElCommand eid "/click" methodPost Null

elementClear :: Text -> SessState ()
elementClear eid =
    ignore $ doSessElCommand eid "/clear" methodPost Null

elementSendKeys :: Text -> Text -> SessState ()
elementSendKeys eid v =
    ignore $ doSessElCommand eid "/value" methodPost $ object ["value" .= [v]]

executeScript :: (FromJSON a) => Text -> [JSArg] -> SessState a
executeScript s args = 
    doSessCommand "/execute" methodPost $ object ["script" .= s, "args" .= args]


elementScreenshot :: Text -> SessState Text
elementScreenshot eid = 
    doSessElCommand eid "/screenshot" methodGet Null 

elementSaveScreenshot :: FilePath -> Text -> SessState ()
elementSaveScreenshot path eid = do
    s <- fmap (B.decodeLenient . T.encodeUtf8) . elementScreenshot $ eid
    liftBase $ B.writeFile path s

acceptAlert :: SessState ()
acceptAlert = ignore $ doSessCommand "/accept_alert" methodPost Null

getAlert :: SessState Text
getAlert = doSessCommand "/alert_text" methodGet Null

-- selectOption :: Text -> Text -> SessState ()
-- selectOption eid v = do
--     l <- findChildrenElement eid (TagName "option")
--     o <- filterM findOption l
--     elementClick . head $ o
--     where
--         findOption a = getElementText a >>= (\x -> return (x == v))