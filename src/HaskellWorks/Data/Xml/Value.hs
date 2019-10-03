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
import HaskellWorks.Data.Xml.RawDecode
import HaskellWorks.Data.Xml.RawValue

data Value
  = XmlDocument
    { _childNodes :: [Value]
    }
  | XmlText
    { _textValue :: String
    }
  | XmlElement
    { _name       :: String
    , _attributes :: [(String, String)]
    , _childNodes :: [Value]
    }
  | XmlCData
    { _cdata :: String
    }
  | XmlComment
    { _comment :: String
    }
  | XmlMeta
    { _name       :: String
    , _childNodes :: [Value]
    }
  | XmlError
    { _errorMessage :: String
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
  rawDecode (RawAttrList  as        ) = XmlError      ("Can't decode attribute list: "  <> show as)
  rawDecode (RawError     msg       ) = XmlError      msg

mkXmlElement :: String -> [RawValue] -> Value
mkXmlElement n (RawAttrList as:cs) = XmlElement n (mkAttrs as) (rawDecode <$> cs)
mkXmlElement n cs                  = XmlElement n []           (rawDecode <$> cs)

mkAttrs :: [RawValue] -> [(String, String)]
mkAttrs (RawAttrName n:RawAttrValue v:cs) = (n, v):mkAttrs cs
mkAttrs (_:cs)                            = mkAttrs cs
mkAttrs []                                = []
