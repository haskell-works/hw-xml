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
  = XmlText String
  | XmlElement String [XmlValue]
  | XmlCData String
  | XmlComment String
  | XmlMeta String
  | XmlAttrList [(String, String)]
  deriving (Eq, Show)

class XmlValueAt a where
  xmlValueAt :: a -> Either DecodeError XmlValue

class FromXmlValue a where
  fromXmlValue :: XmlValue -> Either DecodeError a

instance XmlValueAt XmlIndex where
  xmlValueAt i = case i of
    XmlIndexCData s        -> XmlCData   <$> parseTextUntil "]]>" s
    XmlIndexComment s      -> XmlComment <$> parseTextUntil "-->" s
    XmlIndexMeta s _       -> Right $ XmlMeta s
    XmlIndexElement s cs   -> XmlElement s <$> mapM xmlValueAt cs  --Right $ XmlElement s []
    XmlIndexAttrList as    -> XmlAttrList <$> mapM (\(k, v) -> (,) <$> parseAttrName k <*> parseString v) as
    _                      -> decodeErr "Not yet supported" ""
    -- XmlIndexString s -> case ABC.parse parseXmlString s of
    --   ABC.Fail    {}     -> Left (DecodeError ("Invalid string: '" <> show (BS.take 20 s) <> "...'"))
    --   ABC.Partial _      -> Left (DecodeError "Unexpected end of string")
    --   ABC.Done    _ r    -> Right (XmlText r)
    -- XmlIndexAttrList as  -> XmlAttrList <$> mapM (\(k, v) -> (,) <$> parseString k <*> parseString v) as
    -- XmlIndexElement _ es -> XmlElement <$> mapM xmlValueAt es
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
