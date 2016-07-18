{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}

module HaskellWorks.Data.Xml.Value.Internal where

import           Control.Applicative
import qualified Data.Attoparsec.ByteString.Char8 as ABC
import qualified Data.Attoparsec.Types            as T
import           Data.Bits
import           Data.Char
import           Data.String
import           HaskellWorks.Data.Parser         as P

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
  q <- string "\"" <|> string "'"
  value <- many (verbatimChar <|> escapedChar <|> escapedCode)
  _ <- string q
  return value
 
parseXmlToken :: (P.Parser t, IsString t) => T.Parser t String
parseXmlToken = do
  value <- many string
  _     <- ABC.space <|> char '='
  return value

verbatimChar :: P.Parser t => T.Parser t Char
verbatimChar  = satisfyChar (ABC.notInClass "\"\\") <?> "invalid string character"

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
  return $ chr $ a `shift` 24 .|. b `shift` 16 .|. c `shift` 8 .|. d
