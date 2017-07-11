{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}

module HaskellWorks.Data.Xml.RawValue
( RawValue(..)
, RawValueAt(..)
)
where

import qualified Data.Attoparsec.ByteString.Char8     as ABC
import qualified Data.ByteString                      as BS
import           Data.Monoid
import           Data.List
import           HaskellWorks.Data.Xml.Grammar
import           HaskellWorks.Data.Xml.Succinct.Index
import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>), (<>))

data RawValue
  = XmlDocument [RawValue]
  | XmlText String
  | XmlElement String [RawValue]
  | XmlCData String
  | XmlComment String
  | XmlMeta String [RawValue]
  | XmlAttrName String
  | XmlAttrValue String
  | XmlAttrList [RawValue]
  | XmlError String
  deriving (Eq, Show)

instance Pretty RawValue where
  pretty mjpv = case mjpv of
    XmlText s       -> ctext $ text s
    XmlAttrName s   -> text s
    XmlAttrValue s  -> (ctext . dquotes . text) s
    XmlAttrList ats -> formatAttrs ats
    XmlComment s    -> text $ "<!-- " <> show s <> "-->"
    XmlElement s xs -> formatElem s xs
    XmlDocument xs  -> formatMeta "?" "xml" xs
    XmlError s      -> red $ text "[error " <> text s <> text "]"
    XmlCData s      -> cangle "<!" <> ctag (text "[CDATA[") <> text s <> cangle (text "]]>")
    XmlMeta s xs    -> formatMeta "!" s xs
    where
      formatAttr at = case at of
        XmlAttrName a  -> text " " <> pretty (XmlAttrName a)
        XmlAttrValue a -> text "=" <> pretty (XmlAttrValue a)
        XmlAttrList _ -> red $ text "ATTRS"
        _              -> red $ text "booo"
      formatAttrs ats = hcat (formatAttr <$> ats)
      formatElem s xs =
        let (ats, es) = partition isAttrL xs
        in  cangle langle <> ctag (text s)
              <> hcat (pretty <$> ats)
              <> cangle rangle
              <> hcat (pretty <$> es)
              <> cangle (text "</") <> ctag (text s) <> cangle rangle
      formatMeta b s xs =
        let (ats, es) = partition isAttr xs
        in  cangle (langle <> text b) <> ctag (text s)
              <> hcat (pretty <$> ats)
              <> cangle rangle
              <> hcat (pretty <$> es)

class RawValueAt a where
  rawValueAt :: a -> RawValue

instance RawValueAt XmlIndex where
  rawValueAt i = case i of
    XmlIndexCData s        -> parseTextUntil "]]>" s `as` XmlCData
    XmlIndexComment s      -> parseTextUntil "-->" s `as` XmlComment
    XmlIndexMeta s cs      -> XmlMeta s       (rawValueAt <$> cs)
    XmlIndexElement s cs   -> XmlElement s    (rawValueAt <$> cs)
    XmlIndexDocument cs    -> XmlDocument     (rawValueAt <$> cs)
    XmlIndexAttrName cs    -> parseAttrName cs       `as` XmlAttrName
    XmlIndexAttrValue cs   -> parseString cs         `as` XmlAttrValue
    XmlIndexAttrList cs    -> XmlAttrList     (rawValueAt <$> cs)
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

cangle :: Doc -> Doc
cangle = dullwhite

ctag :: Doc -> Doc
ctag = bold

ctext :: Doc -> Doc
ctext = dullgreen

isAttrL :: RawValue -> Bool
isAttrL (XmlAttrList _) = True
isAttrL _               = False

isAttr :: RawValue -> Bool
isAttr v = case v of
  XmlAttrName  _ -> True
  XmlAttrValue _ -> True
  XmlAttrList  _ -> True
  _              -> False

as :: Either String a -> (a -> RawValue) -> RawValue
as = flip $ either XmlError

decodeErr :: String -> BS.ByteString -> Either String a
decodeErr reason bs =
  Left $ reason <>" (" <> show (BS.take 20 bs) <> "...)"
