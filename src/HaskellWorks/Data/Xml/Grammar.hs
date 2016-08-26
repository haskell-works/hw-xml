{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}

module HaskellWorks.Data.Xml.Grammar where

import           Control.Applicative
--import qualified Data.Attoparsec.ByteString.Char8 as ABC
import qualified Data.Attoparsec.Types    as T
import           Data.Bits
import           Data.Char
import           Data.String
import           HaskellWorks.Data.Parser as P

data XmlElementType
  = XmlElementTypeDocument
  | XmlElementTypeElement String
  | XmlElementTypeComment
  | XmlElementTypeCData
  | XmlElementTypeMeta String

parseHexDigitNumeric :: P.Parser t => T.Parser t Int
parseHexDigitNumeric = do
  c <- satisfyChar (\c -> '0' <= c && c <= '9')
  return $ ord c - ord '0'

parseHexDigitAlphaLower :: P.Parser t => T.Parser t Int
parseHexDigitAlphaLower = do
  c <- satisfyChar (\c -> 'a' <= c && c <= 'z')
  return $ ord c - ord 'a' + 10

parseHexDigitAlphaUpper :: P.Parser t => T.Parser t Int
parseHexDigitAlphaUpper = do
  c <- satisfyChar (\c -> 'A' <= c && c <= 'Z')
  return $ ord c - ord 'A' + 10

parseHexDigit :: P.Parser t => T.Parser t Int
parseHexDigit = parseHexDigitNumeric <|> parseHexDigitAlphaLower <|> parseHexDigitAlphaUpper

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

escapedChar :: (P.Parser t, IsString t) => T.Parser t Char
escapedChar = do
  _ <- string "\\"
  (   char '"'  >> return '"'  ) <|>
    ( char 'b'  >> return '\b' ) <|>
    ( char 'n'  >> return '\n' ) <|>
    ( char 'f'  >> return '\f' ) <|>
    ( char 'r'  >> return '\r' ) <|>
    ( char 't'  >> return '\t' ) <|>
    ( char '\\' >> return '\\' ) <|>
    ( char '\'' >> return '\'' ) <|>
    ( char '/'  >> return '/'  )

escapedCode :: (P.Parser t, IsString t) => T.Parser t Char
escapedCode = do
  _ <- string "\\u"
  a <- parseHexDigit
  b <- parseHexDigit
  c <- parseHexDigit
  d <- parseHexDigit
  return . chr $ a `shift` 24 .|. b `shift` 16 .|. c `shift` 8 .|. d
