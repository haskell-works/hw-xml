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
import Data.Monoid                     ((<>))
import HaskellWorks.Data.Xml.DecodeError
import HaskellWorks.Data.Xml.DecodeResult
import HaskellWorks.Data.Xml.RawDecode
import HaskellWorks.Data.Xml.RawValue
import HaskellWorks.Data.Xml.TagData
import HaskellWorks.Data.Xml.TagInfo

import qualified Data.Map as M

data Value
  = XmlDocument
    { _childNodes :: [Value]
    }
  | XmlText
    { _textValue :: String
    }
  | XmlElement
    { _name       :: String
    , _attributes :: M.Map String String
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
mkXmlElement n cs = case toTagData cs of
  DecodeOk      (TagData      attrs cs' ) -> XmlElement n attrs (rawDecode <$> cs')
  DecodeFailed  (DecodeError  msg       ) -> XmlError msg
