{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module HaskellWorks.Data.Xml.Grammar where

import Control.Applicative
import Data.Char
import Data.String
import Data.Word
import HaskellWorks.Data.Parser as P

import qualified Data.Attoparsec.Types as T

data XmlElementType
  = XmlElementTypeDocument
  | XmlElementTypeElement String
  | XmlElementTypeComment
  | XmlElementTypeCData
  | XmlElementTypeMeta String

parseXmlString :: (P.Parser t Word8) => T.Parser t String
parseXmlString = do
  q <- satisfyChar (=='"') <|> satisfyChar (=='\'')
  many (satisfyChar (/= q))

parseXmlElement :: (P.Parser t Word8, IsString t) => T.Parser t XmlElementType
parseXmlElement = comment <|> cdata <|> doc <|> meta <|> element
  where
  comment = const XmlElementTypeComment  <$> string "!--"
  cdata   = const XmlElementTypeCData    <$> string "![CDATA["
  meta    = XmlElementTypeMeta           <$> (string "!" >> parseXmlToken)
  doc     = const XmlElementTypeDocument <$> string "?xml"
  element = XmlElementTypeElement        <$> parseXmlToken

parseXmlToken :: (P.Parser t Word8) => T.Parser t String
parseXmlToken = many $ satisfyChar isNameChar <?> "invalid string character"

parseXmlAttributeName :: (P.Parser t Word8) => T.Parser t String
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
