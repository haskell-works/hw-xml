{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}

module HaskellWorks.Data.Xml.Value
( XmlValue(..)
, XmlValueAt(..)
, FromXmlValue(..)
)
where

--import           Control.Arrow
import qualified Data.Attoparsec.ByteString.Char8     as ABC
import qualified Data.ByteString                      as BS
import           Data.Monoid
import           HaskellWorks.Data.Decode
import           HaskellWorks.Data.Xml.Grammar
import           HaskellWorks.Data.Xml.Succinct.Index

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
  deriving (Eq, Show)

class XmlValueAt a where
  xmlValueAt :: a -> Either DecodeError XmlValue

class FromXmlValue a where
  fromXmlValue :: XmlValue -> Either DecodeError a

instance XmlValueAt XmlIndex where
  xmlValueAt i = case i of
    XmlIndexCData s        -> XmlCData     <$> parseTextUntil "]]>" s
    XmlIndexComment s      -> XmlComment   <$> parseTextUntil "-->" s
    XmlIndexMeta s cs      -> XmlMeta s    <$> mapM xmlValueAt cs
    XmlIndexElement s cs   -> XmlElement s <$> mapM xmlValueAt cs
    XmlIndexDocument cs    -> XmlDocument  <$> mapM xmlValueAt cs
    XmlIndexAttrName cs    -> XmlAttrName  <$> parseAttrName cs
    XmlIndexAttrValue cs   -> XmlAttrValue <$> parseString cs
    XmlIndexAttrList as    ->
      XmlAttrList  <$> mapM (\(XmlIndexAttrName k, XmlIndexAttrValue v) -> (,) <$> parseAttrName k <*> parseString v) as
    XmlIndexValue s        -> XmlText      <$> parseTextUntil "<" s
    unknown                -> decodeErr ("Not yet supported: " <> show unknown) ""
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

instance FromXmlValue XmlValue where
  fromXmlValue = Right

decodeErr :: String -> BS.ByteString -> Either DecodeError x
decodeErr reason bs =
  Left $ DecodeError (reason <>" (" <> show (BS.take 20 bs) <> "...)")
