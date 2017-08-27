module HaskellWorks.Data.Xml.CharLike where

import Data.Word
import Data.Word8 as W

class XmlCharLike c where
  isElementStart :: c -> Bool
  isExclam       :: c -> Bool
  isHyphen       :: c -> Bool
  isOpenBracket  :: c -> Bool
  isQuestion     :: c -> Bool
  isQuote        :: c -> Bool
  isSpace        :: c -> Bool

instance XmlCharLike Word8 where
  isElementStart     = (== _less)
  isExclam           = (== _exclam)
  isHyphen           = (== _hyphen)
  isOpenBracket      = (== _bracketleft)
  isQuestion         = (== _question)
  isQuote c          = c == _quotedbl || c == _quotesingle
  isSpace            = W.isSpace
