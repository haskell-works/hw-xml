module HaskellWorks.Data.Xml.Conduit.Words where

import           Data.Word
import           Data.Word8

isLeadingDigit :: Word8 -> Bool
isLeadingDigit w = w == _hyphen || (w >= _0 && w <= _9)

isTrailingDigit :: Word8 -> Bool
isTrailingDigit w = w == _plus || w == _hyphen || (w >= _0 && w <= _9) || w == _period || w == _E || w == _e

isAlphabetic :: Word8 -> Bool
isAlphabetic w = (w >= _A && w <= _Z) || (w >= _a && w <= _z)

isQuote :: Word8 -> Bool
isQuote w = w == _quotedbl || w == _quotesingle

isNameStartChar :: Word8 -> Bool
isNameStartChar w = w == _underscore || w == _colon || isAlphabetic w
                 || w `isIn` (0xc0, 0xd6)
                 || w `isIn` (0xd8, 0xf6)
                 || w `isIn` (0xf8, 0xff)

isNameChar :: Word8 -> Bool
isNameChar w = isNameStartChar w || w == _hyphen || w == _period
            || w == 0xb7 || w `isIn` (0, 9)

isXml :: Word8 -> Bool
isXml w = w == _less || w == _greater

isTextStart :: Word8 -> Bool
isTextStart w = not (isSpace w) && w /= _less && w /= _greater

isIn :: Word8 -> (Word8, Word8) -> Bool
isIn w (s, e) = w >= s && w <= e
{-# INLINE isIn #-}

