{-# LANGUAGE BangPatterns #-}

module HaskellWorks.Data.Xml.Extract
  ( extractXmlSnippet
  ) where

import qualified Data.ByteString as BS
import           Data.Word8
import           HaskellWorks.Data.Conduit.Json.Words
import           HaskellWorks.Data.Xml.Type

data ExtractXmlState
  = ExtractXmlStringEscaped
  | ExtractXmlStringInXml
  | ExtractXmlStringInNumber
  | ExtractXmlStringInString

extractXmlSnippet :: BS.ByteString -> Maybe (XmlType, BS.ByteString)
extractXmlSnippet bs = case extractXmlSnippet' 0 ExtractXmlStringInXml bs of
  Just (xmlType, len)  -> Just (xmlType, BS.take len bs)
  Nothing               -> Nothing

extractXmlSnippet' :: Int -> ExtractXmlState -> BS.ByteString -> Maybe (XmlType, Int)
extractXmlSnippet' n ExtractXmlStringInXml bs = case BS.uncons bs of
  Just (!c, !cs) | isLeadingDigit c   -> extractXmlSnippet' (n + 1) ExtractXmlStringInNumber cs
  Just (!c, !cs) | c == _quotedbl     -> extractXmlSnippet' (n + 1) ExtractXmlStringInString cs
  Just (!c,   _) | c == _t            -> Just (XmlTypeBool, n + 1)
  Just (!c,   _) | c == _f            -> Just (XmlTypeBool, n + 1)
  Just (!c,   _) | c == _n            -> Just (XmlTypeNull, n + 1)
  Just (!c,   _) | c == _braceleft    -> Just (XmlTypeObject, n + 1)
  Just (!c,   _) | c == _bracketleft  -> Just (XmlTypeArray, n + 1)
  Just _                              -> Nothing
  Nothing                             -> Nothing
extractXmlSnippet' n ExtractXmlStringInString bs = case BS.uncons bs of
  Just (!c, !cs) | c == _backslash    -> extractXmlSnippet' (n + 1) ExtractXmlStringEscaped  cs
  Just (!c,   _) | c == _quotedbl     -> Just (XmlTypeString, n + 1)
  Just (_ , !cs)                      -> extractXmlSnippet' (n + 1) ExtractXmlStringInString cs
  Nothing                             -> Nothing
extractXmlSnippet' n ExtractXmlStringEscaped bs = case BS.uncons bs of
  Just (_, !cs)                       -> extractXmlSnippet' (n + 1) ExtractXmlStringInString cs
  Nothing                             -> Nothing
extractXmlSnippet' n ExtractXmlStringInNumber bs = case BS.uncons bs of
  Just (!c, !cs) | isTrailingDigit c  -> extractXmlSnippet' (n + 1) ExtractXmlStringInNumber cs
  Just _                              -> Just (XmlTypeNumber, n)
  Nothing                             -> Just (XmlTypeNumber, n)
