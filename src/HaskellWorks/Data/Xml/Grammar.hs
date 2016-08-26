{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}

module HaskellWorks.Data.Xml.Grammar where

import           Control.Applicative
import qualified Data.Attoparsec.Types    as T
import           Data.Char
import           Data.String
import           HaskellWorks.Data.Parser as P

data XmlElementType
  = XmlElementTypeDocument
  | XmlElementTypeElement String
  | XmlElementTypeComment
  | XmlElementTypeCData
  | XmlElementTypeMeta String

parseXmlString :: (P.Parser t, IsString t) => T.Parser t String
parseXmlString = do
  q <- satisfyChar (=='"') <|> satisfyChar (=='\'')
  many (satisfyChar (/= q))

parseXmlElement :: (P.Parser t, IsString t) => T.Parser t XmlElementType
parseXmlElement = comment <|> cdata <|> doc <|> meta <|> element
  where
  comment = const XmlElementTypeComment  <$> string "!--"
  cdata   = const XmlElementTypeCData    <$> string "![CDATA["
  meta    = XmlElementTypeMeta           <$> (string "!" >> parseXmlToken)
  doc     = const XmlElementTypeDocument <$> string "?xml"
  element = XmlElementTypeElement        <$> parseXmlToken

parseXmlToken :: (P.Parser t, IsString t) => T.Parser t String
parseXmlToken = many $ satisfyChar isNameChar <?> "invalid string character"

parseXmlAttributeName :: (P.Parser t, IsString t) => T.Parser t String
parseXmlAttributeName = parseXmlToken

isNameStartChar :: Char -> Bool
isNameStartChar w =
  let iw = ord w
  in w == '_' || w == ':' || isAlpha w
     || (iw >= 0xc0 && iw <= 0xd6)
     || (iw >= 0xd8 && iw <= 0xf6)
     || (iw >= 0xf8 && iw <= 0xff)

isNameChar :: Char -> Bool
isNameChar w = isNameStartChar w || w == '-' || w == '.'
             || ord w == 0xb7 || isNumber w
