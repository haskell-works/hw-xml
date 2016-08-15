{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}

module HaskellWorks.Data.Xml.Value where

--import           Control.Arrow
import qualified Data.Attoparsec.ByteString.Char8     as ABC
import qualified Data.ByteString                      as BS
import           Data.Monoid
import           HaskellWorks.Data.Decode
import           HaskellWorks.Data.Xml.Succinct.Index
import           HaskellWorks.Data.Xml.Grammar

data XmlValue
  = XmlString String
  | XmlElement [XmlValue]
  | XmlAttrList [(String, String)]
  deriving (Eq, Show)

class XmlValueAt a where
  xmlValueAt :: a -> Either DecodeError XmlValue

class FromXmlValue a where
  fromXmlValue :: XmlValue -> Either DecodeError a

instance XmlValueAt XmlIndex where
  xmlValueAt i = case i of
    XmlIndexString s -> case ABC.parse parseXmlString s of
      ABC.Fail    {}     -> Left (DecodeError ("Invalid string: '" <> show (BS.take 20 s) <> "...'"))
      ABC.Partial _      -> Left (DecodeError "Unexpected end of string")
      ABC.Done    _ r    -> Right (XmlString r)
    XmlIndexAttrList as  -> XmlAttrList <$> mapM (\(k, v) -> (,) <$> parseString k <*> parseString v) as
    XmlIndexElement _ es -> XmlElement <$> mapM xmlValueAt es
    where
      parseString bs = case ABC.parse parseXmlString bs of
        ABC.Fail    {}  -> Left (DecodeError ("Invalid field: '" <> show (BS.take 20 bs) <> "...'"))
        ABC.Partial _   -> Left (DecodeError "Unexpected end of field")
        ABC.Done    _ s -> Right s

instance FromXmlValue XmlValue where
  fromXmlValue = Right
