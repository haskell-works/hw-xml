{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}

module HaskellWorks.Data.Xml.Value
  ( Value(..)
  ) where

import Data.Monoid                     ((<>))
import HaskellWorks.Data.Xml.DecodeError
import HaskellWorks.Data.Xml.DecodeResult
import HaskellWorks.Data.Xml.RawDecode
import HaskellWorks.Data.Xml.RawValue
import HaskellWorks.Data.Xml.TagData
import HaskellWorks.Data.Xml.TagInfo

import qualified Data.Map as M

data Value
  = XmlDocument [Value]
  | XmlText String
  | XmlElement String (M.Map String String) [Value]
  | XmlCData String
  | XmlComment String
  | XmlMeta String [Value]
  | XmlError String
  deriving (Eq, Show)

instance RawDecode Value where
  rawDecode (RawDocument  rvs     ) = XmlDocument   (rawDecode <$> rvs)
  rawDecode (RawText      text    ) = XmlText       text
  rawDecode (RawElement   name cs ) = mkXmlElement  name cs
  rawDecode (RawCData     text    ) = XmlCData      text
  rawDecode (RawComment   text    ) = XmlComment    text
  rawDecode (RawMeta      name cs ) = XmlMeta       name (rawDecode <$> cs)
  rawDecode (RawAttrName  name    ) = XmlError      ("Can't decode attribute name: "  <> name   )
  rawDecode (RawAttrValue value   ) = XmlError      ("Can't decode attribute value: " <> value  )
  rawDecode (RawAttrList  as      ) = XmlError      ("Can't decode attribute list: "  <> show as)
  rawDecode (RawError     msg     ) = XmlError      msg

mkXmlElement :: String -> [RawValue] -> Value
mkXmlElement tagName children = case toTagData children of
  DecodeOk      (TagData attrs children') -> XmlElement tagName attrs (rawDecode <$> children')
  DecodeFailed  (DecodeError msg        ) -> XmlError msg
