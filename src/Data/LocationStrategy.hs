{-# LANGUAGE OverloadedStrings #-}
module Data.LocationStrategy where

import Data.Text
import Data.Aeson

data LocationStrategy =     ID              Text
                          | Name            Text
                          | ClassName       Text
                          | CssSelector     Text
                          | LinkText        Text
                          | PartialLinkText Text
                          | TagName         Text
                          | XPath           Text
                          deriving (Eq, Show)

instance ToJSON LocationStrategy where
    toJSON s = case s of
        ID              t -> setSelector "id" t
        Name            t -> setSelector "name" t
        ClassName       t -> setSelector "class name" t
        CssSelector     t -> setSelector "css selector" t
        LinkText        t -> setSelector "link text" t
        PartialLinkText t -> setSelector "partial link text" t
        TagName         t -> setSelector "tag name" t
        XPath           t -> setSelector "xpath" t
        where
            setSelector :: Text -> Text -> Value
            setSelector str v = object ["using" .= str, "value" .= v]