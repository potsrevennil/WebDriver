{-# LANGUAGE OverloadedStrings #-}
module Exceptions where

import Data.ByteString
import Data.Text.Lazy (Text)
import Data.Typeable
import Control.Exception (Exception)

instance Exception BadJSON
newtype BadJSON = BadJSON String
             deriving (Eq, Show, Typeable)

data HTTPStatusUnknown = HTTPStatusUnknown Int ByteString deriving (Eq, Show, Typeable)
instance Exception HTTPStatusUnknown

data ErrorCode =  NoSuchElement                     -- 7
                | NoSuchFrame                       -- 8
                | UnknownCommand                    -- 9
                | StaleElementReference             -- 10
                | ElementNotVisible                 -- 11
                | InvalidElementState               -- 12
                | UnknownError                      -- 13
                | ElementIsNotSelectable            -- 15
                | JavascriptError                   -- 17
                | XPathLookupError                  -- 19
                | Timeout                           -- 21
                | NoSuchWindow                      -- 23
                | InvalidCookieDomain               -- 24
                | UnableToSetCookie                 -- 25
                | UnexpectedAlertOpen               -- 26
                | NoAlertOpen                       -- 27
                | ScriptTimeout                     -- 28
                | InvalidElementCoordinates         -- 29
                | IMENotAvailable                   -- 30
                | IMEEngineActivationFailed         -- 31
                | InvalidSelector                   -- 32
                | SessionNotCreated                 -- 33
                | MoveTargetOutOfBounds             -- 34
                | InvalidXPathSelector              -- 51
                | InvalidXPathSelectorReturnType    -- 52
                | ElementNotInteractable            -- 60
                | InsecureCertificate               
                | InvalidArgument                   -- 61
                | InvalidCoordinates                
                | InvalidSessionId
                | NoSuchCookie                      -- 62
                | UnableToCaptureScreen             -- 63
                | ElementClickIntercepted           -- 64
                | UnknownMethod
                | MethodNotAllowed                  -- 405
                       deriving (Eq, Show)

instance Exception ErrorCode

toErrorCode :: (Maybe Integer, Maybe Text) -> ErrorCode
toErrorCode e 
    | e == (Just 7,   Just "no such element")               = NoSuchElement
    | e == (Just 8,   Just "no such frame")                 = NoSuchFrame
    | e == (Just 9,   Just "unknown command")               = UnknownCommand
    | e == (Just 10,  Just "stale element reference")       = StaleElementReference
    | e == (Just 11,  Just "element not visible")           = ElementNotVisible
    | e == (Just 12,  Just "invalid element state")         = InvalidElementState
    | e == (Just 13,  Just "unknown error")                 = UnknownError
    | e == (Just 15,  Just "element not selectable")        = ElementIsNotSelectable
    | e == (Just 17,  Just "javascript error")              = JavascriptError
    | e == (Just 19,  Just "invalid selector")              = InvalidSelector
    | e == (Just 21,  Just "timeout")                       = Timeout
    | e == (Just 23,  Just "no such window")                = NoSuchWindow
    | e == (Just 24,  Just "invalid cookie domain")         = InvalidCookieDomain
    | e == (Just 25,  Just "unable to set cookie")          = UnableToSetCookie
    | e == (Just 26,  Just "unexpected alert open")         = UnexpectedAlertOpen
    | e == (Just 27,  Just "no such alert")                 = NoAlertOpen
    | e == (Just 28,  Just "script timeout")                = ScriptTimeout
    | e == (Just 29,  Just "invalid element coordinates")   = InvalidElementCoordinates
    | e == (Just 30,  Just "ime not available")             = IMENotAvailable
    | e == (Just 31,  Just "ime engine activation failed")  = IMEEngineActivationFailed
    | e == (Just 32,  Just "invalid selector")              = InvalidSelector
    | e == (Just 33,  Just "session not created")           = SessionNotCreated
    | e == (Just 34,  Just "move target out of bounds")     = MoveTargetOutOfBounds
    | e == (Just 51,  Just "invalid selector")              = InvalidXPathSelector
    | e == (Just 52,  Just "invalid selector")              = InvalidXPathSelectorReturnType
    | e == (Just 60,  Just "element not interactable")      = ElementNotInteractable
    | e == (Nothing,  Just "insecure certificate")          = InsecureCertificate
    | e == (Just 61,  Just "invalid argument")              = InvalidArgument
    | e == (Nothing,  Just "invalid coordinates")           = InvalidCoordinates
    | e == (Nothing,  Just "invalid session id")            = InvalidSessionId
    | e == (Just 62,  Just "no such cookie")                = NoSuchCookie
    | e == (Just 63,  Just "unable to capture screen")      = UnableToCaptureScreen
    | e == (Just 64,  Just "element click intercepted")     = ElementClickIntercepted
    | e == (Nothing,  Just "unknown method exception")      = UnknownMethod
    | e == (Just 405, Just "unsupported operation")         = MethodNotAllowed
    | otherwise                                             = UnknownError