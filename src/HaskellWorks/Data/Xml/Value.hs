{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}

module HaskellWorks.Data.Xml.Value
  ( Value(..)
  , HasValue(..)
  , _XmlDocument
  , _XmlText
  , _XmlElement
  , _XmlCData
  , _XmlComment
  , _XmlMeta
  , _XmlError
  ) where

import Control.Lens
import Data.Text                           (Text)
import HaskellWorks.Data.Xml.Internal.Show
import HaskellWorks.Data.Xml.RawDecode
import HaskellWorks.Data.Xml.RawValue

data Value
  = XmlDocument
    { _childNodes :: [Value]
    }
  | XmlText
    { _textValue :: Text
    }
  | XmlElement
    { _name       :: Text
    , _attributes :: [(Text, Text)]
    , _childNodes :: [Value]
    }
  | XmlCData
    { _cdata :: Text
    }
  | XmlComment
    { _comment :: Text
    }
  | XmlMeta
    { _name       :: Text
    , _childNodes :: [Value]
    }
  | XmlError
    { _errorMessage :: Text
    }
  deriving (Eq, Show)

makeClassy ''Value
makePrisms ''Value

instance RawDecode Value where
  rawDecode (RawDocument  rvs       ) = XmlDocument   (rawDecode <$> rvs)
  rawDecode (RawText      text      ) = XmlText       text
  rawDecode (RawElement   n cs      ) = mkXmlElement  n cs
  rawDecode (RawCData     text      ) = XmlCData      text
  rawDecode (RawComment   text      ) = XmlComment    text
  rawDecode (RawMeta      n cs      ) = XmlMeta       n (rawDecode <$> cs)
  rawDecode (RawAttrName  nameValue ) = XmlError      ("Can't decode attribute name: "  <> nameValue)
  rawDecode (RawAttrValue attrValue ) = XmlError      ("Can't decode attribute value: " <> attrValue)
  rawDecode (RawAttrList  as        ) = XmlError      ("Can't decode attribute list: "  <> tshow as)
  rawDecode (RawError     msg       ) = XmlError      msg

mkXmlElement :: Text -> [RawValue] -> Value
mkXmlElement n (RawAttrList as:cs) = XmlElement n (mkAttrs as) (rawDecode <$> cs)
mkXmlElement n cs                  = XmlElement n []           (rawDecode <$> cs)

mkAttrs :: [RawValue] -> [(Text, Text)]
mkAttrs (RawAttrName n:RawAttrValue v:cs) = (n, v):mkAttrs cs
mkAttrs (_:cs)                            = mkAttrs cs
mkAttrs []                                = []
