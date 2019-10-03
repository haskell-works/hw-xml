{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module HaskellWorks.Data.Xml.RawValue
  ( RawValue(..)
  , RawValueAt(..)
  ) where

import Data.List
import HaskellWorks.Data.Xml.Grammar
import HaskellWorks.Data.Xml.Succinct.Index
import Text.PrettyPrint.ANSI.Leijen         hiding ((<$>), (<>))

import qualified Data.Attoparsec.ByteString.Char8 as ABC
import qualified Data.ByteString                  as BS

data RawValue
  = RawDocument [RawValue]
  | RawText String
  | RawElement String [RawValue]
  | RawCData String
  | RawComment String
  | RawMeta String [RawValue]
  | RawAttrName String
  | RawAttrValue String
  | RawAttrList [RawValue]
  | RawError String
  deriving (Eq, Show)

instance Pretty RawValue where
  pretty mjpv = case mjpv of
    RawText s       -> ctext $ text s
    RawAttrName s   -> text s
    RawAttrValue s  -> (ctext . dquotes . text) s
    RawAttrList ats -> formatAttrs ats
    RawComment s    -> text $ "<!-- " <> show s <> "-->"
    RawElement s xs -> formatElem s xs
    RawDocument xs  -> formatMeta "?" "xml" xs
    RawError s      -> red $ text "[error " <> text s <> text "]"
    RawCData s      -> cangle "<!" <> ctag (text "[CDATA[") <> text s <> cangle (text "]]>")
    RawMeta s xs    -> formatMeta "!" s xs
    where
      formatAttr at = case at of
        RawAttrName a  -> text " " <> pretty (RawAttrName a)
        RawAttrValue a -> text "=" <> pretty (RawAttrValue a)
        RawAttrList _  -> red $ text "ATTRS"
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
    XmlIndexCData s      -> parseTextUntil "]]>" s `as` RawCData
    XmlIndexComment s    -> parseTextUntil "-->" s `as` RawComment
    XmlIndexMeta s cs    -> RawMeta s       (rawValueAt <$> cs)
    XmlIndexElement s cs -> RawElement s    (rawValueAt <$> cs)
    XmlIndexDocument cs  -> RawDocument     (rawValueAt <$> cs)
    XmlIndexAttrName cs  -> parseAttrName cs       `as` RawAttrName
    XmlIndexAttrValue cs -> parseString cs         `as` RawAttrValue
    XmlIndexAttrList cs  -> RawAttrList     (rawValueAt <$> cs)
    XmlIndexValue s      -> parseTextUntil "<" s   `as` RawText
    XmlIndexError s      -> RawError s
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
isAttrL (RawAttrList _) = True
isAttrL _               = False

isAttr :: RawValue -> Bool
isAttr v = case v of
  RawAttrName  _ -> True
  RawAttrValue _ -> True
  RawAttrList  _ -> True
  _              -> False

as :: Either String a -> (a -> RawValue) -> RawValue
as = flip $ either RawError

decodeErr :: String -> BS.ByteString -> Either String a
decodeErr reason bs =
  Left $ reason <>" (" <> show (BS.take 20 bs) <> "...)"
