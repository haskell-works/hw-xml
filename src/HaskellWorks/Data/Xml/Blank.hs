{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.Data.Xml.Blank
  ( blankXml
  ) where

import Data.ByteString                      (ByteString)
import Data.Word
import Data.Word8
import HaskellWorks.Data.Xml.Internal.Words
import Prelude

import qualified Data.ByteString as BS

type ExpectedChar = Word8

data BlankState
  = InXml
  | InTag
  | InAttrList
  | InCloseTag
  | InClose
  | InBang Int
  | InString ExpectedChar
  | InText
  | InMeta
  | InCdataTag
  | InCdata Int
  | InRem Int
  | InIdent
  deriving (Eq, Show)

data ByteStringP = BSP Word8 ByteString | EmptyBSP deriving Show

blankXml :: BS.ByteString -> BS.ByteString
blankXml as = fst (BS.unfoldrN (BS.length as) go (InXml, as))
  where go :: (BlankState, ByteString) -> Maybe (Word8, (BlankState, ByteString))
        go (InXml, bs) = case BS.uncons bs of
          Just (!c, !cs) | isMetaStart c cs -> Just (_bracketleft , (InMeta       , cs))
          Just (!c, !cs) | isEndTag c cs    -> Just (_space       , (InCloseTag   , cs))
          Just (!c, !cs) | isTextStart c    -> Just (_t           , (InText       , cs))
          Just (!c, !cs) | c == _less       -> Just (_less        , (InTag        , cs))
          Just (!c, !cs) | isSpace c        -> Just (c            , (InXml        , cs))
          Just ( _, !cs)                    -> Just (_space       , (InXml        , cs))
          Nothing                           -> Nothing
        go (InTag, bs) = case BS.uncons bs of
          Just (!c, !cs) | isSpace c       -> Just (_parenleft   , (InAttrList   , cs))
          Just (!c, !cs) | isTagClose c cs -> Just (_space       , (InClose      , cs))
          Just (!c, !cs) | c == _greater   -> Just (_space       , (InXml        , cs))
          Just (!c, !cs) | isSpace c       -> Just (c            , (InTag        , cs))
          Just ( _, !cs)                   -> Just (_space       , (InTag        , cs))
          Nothing                          -> Nothing
        go (InCloseTag, bs) = case BS.uncons bs of
          Just (!c, !cs) | c == _greater -> Just (_greater     , (InXml        , cs))
          Just ( _, !cs)                 -> Just (_space       , (InCloseTag   , cs))
          Nothing                        -> Nothing
        go (InAttrList, bs) = case BS.uncons bs of
          Just (!c, !cs) | c == _greater     -> Just (_parenright  , (InXml        , cs))
          Just (!c, !cs) | isTagClose c cs   -> Just (_parenright  , (InClose      , cs))
          Just (!c, !cs) | isNameStartChar c -> Just (_a           , (InIdent      , cs))
          Just (!c, !cs) | isQuote c         -> Just (_v           , (InString c   , cs))
          Just (!c, !cs) | isSpace c         -> Just (c            , (InAttrList   , cs))
          Just ( _, !cs)                     -> Just (_space       , (InAttrList   , cs))
          Nothing                            -> Nothing
        go (InClose, bs) = case BS.uncons bs of
          Just (_, !cs) -> Just (_greater     , (InXml        , cs))
          Nothing       -> Nothing
        go (InIdent, bs) = case BS.uncons bs of
          Just (!c, !cs) | isNameChar c -> Just (_space       , (InIdent      , cs))
          Just (!c, !cs) | isSpace c    -> Just (_space       , (InAttrList   , cs))
          Just (!c, !cs) | c == _equal  -> Just (_space       , (InAttrList   , cs))
          Just (!c, !cs) | isSpace c    -> Just (c            , (InAttrList   , cs))
          Just ( _, !cs)                -> Just (_space       , (InAttrList   , cs))
          Nothing                       -> Nothing
        go (InString q, bs) = case BS.uncons bs of
          Just (!c, !cs) | c == q    -> Just (_space       , (InAttrList   , cs))
          Just (!c, !cs) | isSpace c -> Just (c            , (InString q   , cs))
          Just ( _, !cs)             -> Just (_space       , (InString q   , cs))
          Nothing                    -> Nothing
        go (InText, bs) = case BS.uncons bs of
          Just (!c, !cs) | isEndTag c cs        -> Just (_space       , (InCloseTag   , cs))
          Just ( _, !cs) | headIs (== _less) cs -> Just (_space       , (InXml        , cs))
          Just (!c, !cs) | isSpace c            -> Just (c            , (InText       , cs))
          Just ( _, !cs)                        -> Just (_space       , (InText       , cs))
          Nothing                               -> Nothing
        go (InMeta, bs) = case BS.uncons bs of
          Just (!c, !cs) | c == _exclam      -> Just (_space       , (InMeta       , cs))
          Just (!c, !cs) | c == _hyphen      -> Just (_space       , (InRem 0      , cs))
          Just (!c, !cs) | c == _bracketleft -> Just (_space       , (InCdataTag   , cs))
          Just (!c, !cs) | c == _greater     -> Just (_bracketright, (InXml        , cs))
          Just (!c, !cs) | isSpace c         -> Just (c            , (InBang 1     , cs))
          Just ( _, !cs)                     -> Just (_space       , (InBang 1     , cs))
          Nothing                            -> Nothing
        go (InCdataTag, bs) = case BS.uncons bs of
          Just (!c, !cs) | c == _bracketleft -> Just (_space       , (InCdata 0    , cs))
          Just (!c, !cs) | isSpace c         -> Just (c            , (InCdataTag   , cs))
          Just ( _, !cs)                     -> Just (_space       , (InCdataTag   , cs))
          Nothing                            -> Nothing
        go (InCdata n, bs) = case BS.uncons bs of
          Just (!c, !cs) | c == _greater && n >= 2  -> Just (_bracketright, (InXml        , cs))
          Just (!c, !cs) | isCdataEnd c cs && n > 0 -> Just (_space       , (InCdata (n+1), cs))
          Just (!c, !cs) | c == _bracketright       -> Just (_space       , (InCdata (n+1), cs))
          Just (!c, !cs) | isSpace c                -> Just (c            , (InCdata 0    , cs))
          Just ( _, !cs)                            -> Just (_space       , (InCdata 0    , cs))
          Nothing                                   -> Nothing
        go (InRem n, bs) = case BS.uncons bs of
          Just (!c, !cs) | c == _greater && n >= 2 -> Just (_bracketright, (InXml        , cs))
          Just (!c, !cs) | c == _hyphen            -> Just (_space       , (InRem (n+1)  , cs))
          Just (!c, !cs) | isSpace c               -> Just (c            , (InRem 0      , cs))
          Just ( _, !cs)                           -> Just (_space       , (InRem 0      , cs))
          Nothing                                  -> Nothing
        go (InBang n, bs) = case BS.uncons bs of
          Just (!c, !cs) | c == _less              -> Just (_bracketleft , (InBang (n+1) , cs))
          Just (!c, !cs) | c == _greater && n == 1 -> Just (_bracketright, (InXml        , cs))
          Just (!c, !cs) | c == _greater           -> Just (_bracketright, (InBang (n-1) , cs))
          Just (!c, !cs) | isSpace c               -> Just (c            , (InBang n     , cs))
          Just ( _, !cs)                           -> Just (_space       , (InBang n     , cs))
          Nothing                                  -> Nothing

isEndTag :: Word8 -> ByteString -> Bool
isEndTag c cs = c == _less && headIs (== _slash) cs
{-# INLINE isEndTag #-}

isTagClose :: Word8 -> ByteString -> Bool
isTagClose c cs = (c == _slash) || ((c == _slash || c == _question) && headIs (== _greater) cs)
{-# INLINE isTagClose #-}

isMetaStart :: Word8 -> ByteString -> Bool
isMetaStart c cs = c == _less && headIs (== _exclam) cs
{-# INLINE isMetaStart #-}

isCdataEnd :: Word8 -> ByteString -> Bool
isCdataEnd c cs = c == _bracketright && headIs (== _greater) cs
{-# INLINE isCdataEnd #-}

headIs :: (Word8 -> Bool) -> ByteString -> Bool
headIs p bs = case BS.uncons bs of
  Just (!c, _) -> p c
  Nothing      -> False
{-# INLINE headIs #-}
