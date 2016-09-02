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
--import           Data.List
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
  | XmlAttrList [XmlValue]
  | XmlError String
  deriving (Eq, Show)

-- instance Pretty XmlValue where
--   pretty mjpv = case mjpv of
--     XmlText s       -> dullgreen   (text (show s))
--     XmlAttrName s   -> cyan        (text (show s))
--     XmlAttrValue s  -> red.dquotes (text (show s))
--     XmlAttrList as  -> hsep        (attrPair <$> as)
--     XmlComment s    -> dullblack   (text $ "<!-- " <> show s <> "-->")
--     XmlElement s xs -> encloseSep (cyan $ langle <> text s)
--
--     -- JsonPartialObject kvs -> hEncloseSep (text "{") (text "}") (text ",") ((pretty . toJsonPartialField) `map` kvs)
--     -- JsonPartialArray vs   -> hEncloseSep (text "[") (text "]") (text ",") (pretty `map` vs)
--     -- JsonPartialBool w     -> red (text (show w))
--     -- JsonPartialNull       -> text "null"
--     XmlError s    -> text "[error " <> text s <> text "]"
--     where
--       attrPair (k, v) = (cyan.text $ show k) <> equals <> (red.dquotes.text $ show v)
--       elem1 s xs =
--         let (as, es) = partition isAttr xs
--             attrs = (\(k, v) -> (XmlIndexAttrName k, )
--         in  cyan (langle <> text s) <> (hsep (attrPair <$> as))
--       isAttr v = case v of
--         XmlAttrName  _ -> True
--         XmlAttrValue _ -> True
--         XmlAttrList  _ -> True
--         _              -> False

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
    XmlIndexAttrList cs    -> XmlAttrList     (xmlValueAt <$> cs)
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

as :: Either String a -> (a -> XmlValue) -> XmlValue
as = flip $ either XmlError

decodeErr :: String -> BS.ByteString -> Either String a
decodeErr reason bs =
  Left $ reason <>" (" <> show (BS.take 20 bs) <> "...)"
