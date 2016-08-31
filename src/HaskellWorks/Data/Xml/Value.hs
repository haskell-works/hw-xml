{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}

module HaskellWorks.Data.Xml.Value
( XmlValue(..)
, XmlValueAt(..)
)
where

import qualified Data.Attoparsec.ByteString.Char8     as ABC
import qualified Data.ByteString                      as BS
import           Data.Monoid
import           HaskellWorks.Data.Xml.Grammar
import           HaskellWorks.Data.Xml.Succinct.Index
--import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>), (<>))

data XmlValue
  = XmlDocument [XmlValue]
  | XmlText String
  | XmlElement String [XmlValue]
  | XmlCData String
  | XmlComment String
  | XmlMeta String [XmlValue]
  | XmlAttrName String
  | XmlAttrValue String
  | XmlAttrList [(String, String)]
  | XmlError String
  deriving (Eq, Show)

class XmlValueAt a where
  xmlValueAt :: a -> XmlValue

instance XmlValueAt XmlIndex where
  xmlValueAt i = case i of
    XmlIndexCData s        -> parseTextUntil "]]>" s `as` XmlCData
    XmlIndexComment s      -> parseTextUntil "-->" s `as` XmlComment
    XmlIndexMeta s cs      -> XmlMeta s       (xmlValueAt <$> cs)
    XmlIndexElement s cs   -> XmlElement s    (xmlValueAt <$> cs)
    XmlIndexDocument cs    -> XmlDocument     (xmlValueAt <$> cs)
    XmlIndexAttrName cs    -> parseAttrName cs       `as` XmlAttrName
    XmlIndexAttrValue cs   -> parseString cs         `as` XmlAttrValue
    XmlIndexAttrList cs    -> mapM parseAttrKV cs    `as` XmlAttrList
    XmlIndexValue s        -> parseTextUntil "<" s   `as` XmlText
    XmlIndexError s        -> XmlError s
    --unknown                -> XmlError ("Not yet supported: " <> show unknown)
    where
      parseUntil s = ABC.manyTill ABC.anyChar (ABC.string s)

      parseTextUntil s bs = case ABC.parse (parseUntil s) bs of
        ABC.Fail    {}  -> decodeErr ("Unable to find " <> show s <> ".") bs
        ABC.Partial _   -> decodeErr ("Unexpected end, expected " <> show s <> ".") bs
        ABC.Done    _ r -> Right r
      parseString bs = case ABC.parse parseXmlString bs of
        ABC.Fail    {}  -> decodeErr "Unable to parse string" bs
        ABC.Partial _   -> decodeErr "Unexpected end of string, expected" bs
        ABC.Done    _ r -> Right r
      parseAttrName bs = case ABC.parse parseXmlAttributeName bs of
        ABC.Fail    {}  -> decodeErr "Unable to parse attribute name" bs
        ABC.Partial _   -> decodeErr "Unexpected end of attr name, expected" bs
        ABC.Done    _ r -> Right r

      parseAttrKV (XmlIndexAttrName k, XmlIndexAttrValue v) = (,) <$> parseAttrName k <*> parseString v
      parseAttrKV _ = Left "Unable to parse attribute list"

as :: Either String a -> (a -> XmlValue) -> XmlValue
as = flip $ either XmlError

decodeErr :: String -> BS.ByteString -> Either String a
decodeErr reason bs =
  Left $ reason <>" (" <> show (BS.take 20 bs) <> "...)"
