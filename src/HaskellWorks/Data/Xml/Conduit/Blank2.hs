{-# OPTIONS_GHC-funbox-strict-fields #-}
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.Data.Xml.Conduit.Blank2
  ( blankXml
  , BlankData(..)
  ) where

import Control.Monad
import Control.Monad.Trans.Resource        (MonadThrow)
import Data.ByteString                     as BS
import Data.Conduit
import Data.Monoid ((<>))
import Data.Word
import Data.Word8
import HaskellWorks.Data.Xml.Conduit.Words
import Prelude                             as P

type ExpectedChar      = Word8

data BlankState
  = InXml
  | InTag
  | InAttrList
  | InCloseTag
  | InClose
  | InBang !Int
  | InString !ExpectedChar
  | InText
  | InMeta
  | InCdataTag
  | InCdata !Int
  | InRem !Int
  | InIdent

data BlankData = BlankData
  { blankState  :: !BlankState
  , blankA      :: !Word8
  , blankB      :: !Word8
  , blankC      :: !ByteString
  }

blankXml :: MonadThrow m => Conduit BS.ByteString m BS.ByteString
blankXml = blankXmlPlan1 BS.empty InXml

blankXmlPlan1 :: MonadThrow m => BS.ByteString -> BlankState -> Conduit BS.ByteString m BS.ByteString
blankXmlPlan1 as lastState = do
  mbs <- await
  case mbs of
    Just bs -> do
      let cs = as <> bs
      case BS.uncons cs of
        Just (d, ds) -> case BS.uncons ds of
          Just (e, es) -> blankXmlRun False d e es lastState
          Nothing -> blankXmlPlan1 cs lastState
        Nothing -> blankXmlPlan1 cs lastState
    Nothing -> yield $ BS.map (const _space) as

blankXmlPlan2 :: MonadThrow m => Word8 -> Word8 -> BlankState -> Conduit BS.ByteString m BS.ByteString
blankXmlPlan2 a b lastState = do
  mcs <- await
  case mcs of
    Just cs -> blankXmlRun False a b cs lastState
    Nothing -> blankXmlRun True a b (BS.pack [_space, _space]) lastState

blankXmlRun :: MonadThrow m => Bool -> Word8 -> Word8 -> BS.ByteString -> BlankState -> Conduit BS.ByteString m BS.ByteString
blankXmlRun done a b cs lastState = do
  let (!ds, Just (BlankData !nextState _ _ _)) = unfoldrN (BS.length cs) blankByteString (BlankData lastState a b cs)
  yield ds
  let (yy, zz) = case BS.unsnoc cs of
        Just (ys, z) -> case BS.unsnoc ys of
          Just (_, y) -> (y, z)
          Nothing -> (b, z)
        Nothing -> (a, b)
  unless done (blankXmlPlan2 yy zz nextState)

mkNext :: Word8 -> BlankState -> Word8 -> BS.ByteString -> Maybe (Word8, BlankData)
mkNext w s a bs = case BS.uncons bs of
  Just (b, cs)  -> Just (w, BlankData s a b cs)
  Nothing       -> error "This should never happen"
{-# INLINE mkNext #-}

blankByteString :: BlankData -> Maybe (Word8, BlankData)
blankByteString (BlankData  InXml        a b cs) | isMetaStart a b         = mkNext _bracketleft   InMeta          b cs
blankByteString (BlankData  InXml        a b cs) | isEndTag    a b         = mkNext _space         InCloseTag      b cs
blankByteString (BlankData  InXml        a b cs) | isTextStart a           = mkNext _t             InText          b cs
blankByteString (BlankData  InXml        a b cs) | a == _less              = mkNext _less          InTag           b cs
blankByteString (BlankData  InXml        a b cs) | isSpace a               = mkNext a              InXml           b cs
blankByteString (BlankData  InXml        _ b cs)                           = mkNext _space         InXml           b cs
blankByteString (BlankData  InTag        a b cs) | isSpace a               = mkNext _parenleft     InAttrList      b cs
blankByteString (BlankData  InTag        a b cs) | isTagClose a b          = mkNext _space         InClose         b cs
blankByteString (BlankData  InTag        a b cs) | a == _greater           = mkNext _space         InXml           b cs
blankByteString (BlankData  InTag        a b cs) | isSpace a               = mkNext a              InTag           b cs
blankByteString (BlankData  InTag        _ b cs)                           = mkNext _space         InTag           b cs
blankByteString (BlankData  InCloseTag   a b cs) | a == _greater           = mkNext _greater       InXml           b cs
blankByteString (BlankData  InCloseTag   a b cs) | isSpace a               = mkNext a              InCloseTag      b cs
blankByteString (BlankData  InCloseTag   _ b cs)                           = mkNext _space         InCloseTag      b cs
blankByteString (BlankData  InAttrList   a b cs) | a == _greater           = mkNext _parenright    InXml           b cs
blankByteString (BlankData  InAttrList   a b cs) | isTagClose a b          = mkNext _parenright    InClose         b cs
blankByteString (BlankData  InAttrList   a b cs) | isNameStartChar a       = mkNext _a             InIdent         b cs
blankByteString (BlankData  InAttrList   a b cs) | isQuote a               = mkNext _v             (InString a)    b cs
blankByteString (BlankData  InAttrList   a b cs) | isSpace a               = mkNext a              InAttrList      b cs
blankByteString (BlankData  InAttrList   _ b cs)                           = mkNext _space         InAttrList      b cs
blankByteString (BlankData  InClose      _ b cs)                           = mkNext _greater       InXml           b cs
blankByteString (BlankData  InIdent      a b cs) | isNameChar a            = mkNext _space         InIdent         b cs
blankByteString (BlankData  InIdent      a b cs) | isSpace a               = mkNext _space         InAttrList      b cs
blankByteString (BlankData  InIdent      a b cs) | a == _equal             = mkNext _space         InAttrList      b cs
blankByteString (BlankData  InIdent      a b cs) | isSpace a               = mkNext a              InAttrList      b cs
blankByteString (BlankData  InIdent      _ b cs)                           = mkNext _space         InAttrList      b cs
blankByteString (BlankData (InString q ) a b cs) | a == q                  = mkNext _space         InAttrList      b cs
blankByteString (BlankData (InString q ) a b cs) | isSpace a               = mkNext a              (InString q)    b cs
blankByteString (BlankData (InString q ) _ b cs)                           = mkNext _space         (InString q)    b cs
blankByteString (BlankData  InText       a b cs) | isEndTag a b            = mkNext _space         InCloseTag      b cs
blankByteString (BlankData  InText       _ b cs) | b == _less              = mkNext _space         InXml           b cs
blankByteString (BlankData  InText       a b cs) | isSpace a               = mkNext a              InText          b cs
blankByteString (BlankData  InText       _ b cs)                           = mkNext _space         InText          b cs
blankByteString (BlankData  InMeta       a b cs) | a == _exclam            = mkNext _space         InMeta          b cs
blankByteString (BlankData  InMeta       a b cs) | a == _hyphen            = mkNext _space         (InRem 0)       b cs
blankByteString (BlankData  InMeta       a b cs) | a == _bracketleft       = mkNext _space         InCdataTag      b cs
blankByteString (BlankData  InMeta       a b cs) | a == _greater           = mkNext _bracketright  InXml           b cs
blankByteString (BlankData  InMeta       a b cs) | isSpace a               = mkNext a              (InBang 1)      b cs
blankByteString (BlankData  InMeta       _ b cs)                           = mkNext _space         (InBang 1)      b cs
blankByteString (BlankData  InCdataTag   a b cs) | a == _bracketleft       = mkNext _space         (InCdata 0)     b cs
blankByteString (BlankData  InCdataTag   a b cs) | isSpace a               = mkNext a              InCdataTag      b cs
blankByteString (BlankData  InCdataTag   _ b cs)                           = mkNext _space         InCdataTag      b cs
blankByteString (BlankData (InCdata n  ) a b cs) | a == _greater && n >= 2 = mkNext _bracketright  InXml           b cs
blankByteString (BlankData (InCdata n  ) a b cs) | isCdataEnd a b && n > 0 = mkNext _space         (InCdata (n+1)) b cs
blankByteString (BlankData (InCdata n  ) a b cs) | a == _bracketright      = mkNext _space         (InCdata (n+1)) b cs
blankByteString (BlankData (InCdata _  ) a b cs) | isSpace a               = mkNext a              (InCdata 0)     b cs
blankByteString (BlankData (InCdata _  ) _ b cs)                           = mkNext _space         (InCdata 0)     b cs
blankByteString (BlankData (InRem n    ) a b cs) | a == _greater && n >= 2 = mkNext _bracketright  InXml           b cs
blankByteString (BlankData (InRem n    ) a b cs) | a == _hyphen            = mkNext _space         (InRem (n+1))   b cs
blankByteString (BlankData (InRem _    ) a b cs) | isSpace a               = mkNext a              (InRem 0)       b cs
blankByteString (BlankData (InRem _    ) _ b cs)                           = mkNext _space         (InRem 0)       b cs
blankByteString (BlankData (InBang n   ) a b cs) | a == _less              = mkNext _bracketleft   (InBang (n+1))  b cs
blankByteString (BlankData (InBang n   ) a b cs) | a == _greater && n == 1 = mkNext _bracketright  InXml           b cs
blankByteString (BlankData (InBang n   ) a b cs) | a == _greater           = mkNext _bracketright  (InBang (n-1))  b cs
blankByteString (BlankData (InBang n   ) a b cs) | isSpace a               = mkNext a              (InBang n)      b cs
blankByteString (BlankData (InBang n   ) _ b cs)                           = mkNext _space         (InBang n)      b cs
{-# INLINE blankByteString #-}

isEndTag :: Word8 -> Word8 -> Bool
isEndTag a b = a == _less && b == _slash
{-# INLINE isEndTag #-}

isTagClose :: Word8 -> Word8 -> Bool
isTagClose a b = a == _slash || ((a == _slash || a == _question) && b == _greater)
{-# INLINE isTagClose #-}

isMetaStart :: Word8 -> Word8 -> Bool
isMetaStart a b = a == _less && b == _exclam
{-# INLINE isMetaStart #-}

isCdataEnd :: Word8 -> Word8 -> Bool
isCdataEnd a b = a == _bracketright && b == _greater
{-# INLINE isCdataEnd #-}
