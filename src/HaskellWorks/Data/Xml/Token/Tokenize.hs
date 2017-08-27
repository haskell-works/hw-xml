{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module HaskellWorks.Data.Xml.Token.Tokenize
    ( IsChar(..)
    , XmlToken(..)
    , ParseXml(..)
    ) where

import Control.Applicative
import Data.Bits
import Data.Char
import Data.Word
import Data.Word8
import HaskellWorks.Data.Char.IsChar
import HaskellWorks.Data.Parser          as P
import HaskellWorks.Data.Xml.Token.Types

import qualified Data.Attoparsec.ByteString.Char8 as BC
import qualified Data.Attoparsec.Combinator       as AC
import qualified Data.Attoparsec.Types            as T
import qualified Data.ByteString                  as BS

hexDigitNumeric :: P.Parser t => T.Parser t Int
hexDigitNumeric = do
  c <- satisfyChar (\c -> '0' <= c && c <= '9')
  return $ ord c - ord '0'

hexDigitAlphaLower :: P.Parser t => T.Parser t Int
hexDigitAlphaLower = do
  c <- satisfyChar (\c -> 'a' <= c && c <= 'z')
  return $ ord c - ord 'a' + 10

hexDigitAlphaUpper :: P.Parser t => T.Parser t Int
hexDigitAlphaUpper = do
  c <- satisfyChar (\c -> 'A' <= c && c <= 'Z')
  return $ ord c - ord 'A' + 10

hexDigit :: P.Parser t => T.Parser t Int
hexDigit = hexDigitNumeric <|> hexDigitAlphaLower <|> hexDigitAlphaUpper

class ParseXml t s d where
  parseXmlTokenString     :: T.Parser t (XmlToken s d)
  parseXmlToken           :: T.Parser t (XmlToken s d)
  parseXmlTokenBraceL     :: T.Parser t (XmlToken s d)
  parseXmlTokenBraceR     :: T.Parser t (XmlToken s d)
  parseXmlTokenBracketL   :: T.Parser t (XmlToken s d)
  parseXmlTokenBracketR   :: T.Parser t (XmlToken s d)
  parseXmlTokenComma      :: T.Parser t (XmlToken s d)
  parseXmlTokenColon      :: T.Parser t (XmlToken s d)
  parseXmlTokenWhitespace :: T.Parser t (XmlToken s d)
  parseXmlTokenNull       :: T.Parser t (XmlToken s d)
  parseXmlTokenBoolean    :: T.Parser t (XmlToken s d)
  parseXmlTokenDouble     :: T.Parser t (XmlToken s d)

  parseXmlToken =
    parseXmlTokenString     <|>
    parseXmlTokenBraceL     <|>
    parseXmlTokenBraceR     <|>
    parseXmlTokenBracketL   <|>
    parseXmlTokenBracketR   <|>
    parseXmlTokenComma      <|>
    parseXmlTokenColon      <|>
    parseXmlTokenWhitespace <|>
    parseXmlTokenNull       <|>
    parseXmlTokenBoolean    <|>
    parseXmlTokenDouble

instance ParseXml BS.ByteString String Double where
  parseXmlTokenBraceL   = string "{"      >> return XmlTokenBraceL
  parseXmlTokenBraceR   = string "}"      >> return XmlTokenBraceR
  parseXmlTokenBracketL = string "["      >> return XmlTokenBracketL
  parseXmlTokenBracketR = string "]"      >> return XmlTokenBracketR
  parseXmlTokenComma    = string ","      >> return XmlTokenComma
  parseXmlTokenColon    = string ":"      >> return XmlTokenColon
  parseXmlTokenNull     = string "null"   >> return XmlTokenNull
  parseXmlTokenDouble   = XmlTokenNumber <$> rational

  parseXmlTokenString = do
    _ <- string "\""
    value <- many (verbatimChar <|> escapedChar <|> escapedCode)
    _ <- string "\""
    return $ XmlTokenString value
    where
      verbatimChar  = satisfyChar (BC.notInClass "\"\\") <?> "invalid string character"
      escapedChar   = do
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
      escapedCode :: T.Parser BS.ByteString Char
      escapedCode   = do
        _ <- string "\\u"
        a <- hexDigit
        b <- hexDigit
        c <- hexDigit
        d <- hexDigit
        return . chr $ a `shift` 24 .|. b `shift` 16 .|. c `shift` 8 .|. d

  parseXmlTokenWhitespace = do
    _ <- AC.many1' $ BC.choice [string " ", string "\t", string "\n", string "\r"]
    return XmlTokenWhitespace

  parseXmlTokenBoolean = true <|> false
    where
      true  = string "true"   >> return (XmlTokenBoolean True)
      false = string "false"  >> return (XmlTokenBoolean False)

instance ParseXml BS.ByteString BS.ByteString Double where
  parseXmlTokenBraceL   = string "{"      >> return XmlTokenBraceL
  parseXmlTokenBraceR   = string "}"      >> return XmlTokenBraceR
  parseXmlTokenBracketL = string "["      >> return XmlTokenBracketL
  parseXmlTokenBracketR = string "]"      >> return XmlTokenBracketR
  parseXmlTokenComma    = string ","      >> return XmlTokenComma
  parseXmlTokenColon    = string ":"      >> return XmlTokenColon
  parseXmlTokenNull     = string "null"   >> return XmlTokenNull
  parseXmlTokenDouble   = XmlTokenNumber <$> rational

  parseXmlTokenString = do
    _ <- string "\""
    value <- many (verbatimChar <|> escapedChar <|> escapedCode)
    _ <- string "\""
    return . XmlTokenString $ BS.pack value
    where
      word :: Word8 -> T.Parser BS.ByteString Word8
      word w = satisfy (== w)
      verbatimChar :: T.Parser BS.ByteString Word8
      verbatimChar  = satisfy (\w -> w /= _quotedbl && w /= _backslash) -- <?> "invalid string character"
      escapedChar :: T.Parser BS.ByteString Word8
      escapedChar   = do
        _ <- string "\\"
        (   word _quotedbl    >> return _quotedbl       ) <|>
          ( word _b           >> return 0x08            ) <|>
          ( word _n           >> return _lf             ) <|>
          ( word _f           >> return _np             ) <|>
          ( word _r           >> return _cr             ) <|>
          ( word _t           >> return _tab            ) <|>
          ( word _backslash   >> return _backslash      ) <|>
          ( word _quotesingle >> return _quotesingle    ) <|>
          ( word _slash       >> return _slash          )
      escapedCode :: T.Parser BS.ByteString Word8
      escapedCode   = do
        _ <- string "\\u"
        a <- hexDigit
        b <- hexDigit
        c <- hexDigit
        d <- hexDigit
        return . fromIntegral $ a `shift` 24 .|. b `shift` 16 .|. c `shift` 8 .|. d

  parseXmlTokenWhitespace = do
    _ <- AC.many1' $ BC.choice [string " ", string "\t", string "\n", string "\r"]
    return XmlTokenWhitespace

  parseXmlTokenBoolean = true <|> false
    where
      true  = string "true"   >> return (XmlTokenBoolean True)
      false = string "false"  >> return (XmlTokenBoolean False)
