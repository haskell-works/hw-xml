{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.Data.Xml.Conduit.Blank
  ( blankXml
  ) where

import           Control.Monad
import           Control.Monad.Trans.Resource        (MonadThrow)
import           Data.ByteString                     as BS
import           Data.Conduit
import           Data.Word
import           Data.Word8
import           HaskellWorks.Data.Xml.Conduit.Words
import           Prelude                             as P

data BlankState
  = InXml
  | InElement
  | InAttrList
  | InString Word8
  | InNumber
  | InIdent
  | Test

blankXml :: MonadThrow m => Conduit BS.ByteString m BS.ByteString
blankXml = blankXml' InXml

blankXml' :: MonadThrow m => BlankState -> Conduit BS.ByteString m BS.ByteString
blankXml' lastState = do
  mbs <- await
  case mbs of
    Just bs -> do
      let (!cs, Just (!nextState, _)) = unfoldrN (BS.length bs) blankByteString (lastState, bs)
      yield cs
      blankXml' nextState
    Nothing -> return ()
  where
    blankByteString :: (BlankState, ByteString) -> Maybe (Word8, (BlankState, ByteString))
    blankByteString (InXml, bs) = case BS.uncons bs of
      Just (!c, !cs) | c == _less         -> Just (c           , (InElement , cs))
      Just (!c, !cs)                      -> Just (c           , (InXml     , cs))
      Nothing                             -> Nothing
    blankByteString (InElement, bs) = case BS.uncons bs of
      Just (!c, !cs) | c == _greater      -> Just (c           , (InXml     , cs))
      Just (!c, !cs) | isSpace c          -> Just (_parenleft  , (InAttrList, cs))
      Just (!c,  !cs)                      -> Just (c      , (InElement , cs))
      Nothing                             -> Nothing
    blankByteString (InAttrList, bs) = case BS.uncons bs of
      Just (!c, !cs) | c == _greater      -> Just (_parenright , (InXml     , cs))
      Just (!c, !cs) | c == _slash        -> Just (_parenright , (InElement , cs))
      Just (!c, !cs) | isNameChar c       -> Just (_parenleft  , (InIdent   , cs))
      Just (!c, !cs) | c == _equal        -> Just (c           , (InAttrList, cs))
      Just (!c, !cs) | isQuote c          -> Just (_parenleft  , (InString c, cs))
      Just (!c, !cs)                      -> Just (c           , (InAttrList, cs))
      Nothing                             -> Nothing
    blankByteString (InIdent, bs) = case BS.uncons bs of
      Just (!c, !cs) | isNameChar c       -> Just (c      , (InIdent   , cs))
      Just (!c, !cs) | isSpace c          -> Just (_parenright , (InAttrList, cs))
      Just (!c, !cs) | c == _equal        -> Just (_parenright , (InAttrList, cs))
      Just (_, !cs)                       -> Just (_parenright , (InAttrList, cs))
      Nothing                             -> Nothing
    blankByteString (InString q, bs) = case BS.uncons bs of
      Just (!c, !cs) | c == q             -> Just (_parenright , (InAttrList, cs))
      Just (!c , !cs)                      -> Just (c      , (InString q, cs))
      Nothing                             -> Nothing
    blankByteString (Test, bs) = case BS.uncons bs of
      Just (!c, !cs)                      -> Just (c , (Test, cs))
      Nothing                             -> Nothing

headIs :: (Word8 -> Bool) -> ByteString -> Maybe Bool
headIs p bs = case BS.uncons bs of
  Just (!c, _) -> Just (p c)
  Nothing      -> Nothing
{-# INLINE headIs #-}

unsnocStartElem :: ByteString -> (ByteString, Maybe Word8)
unsnocStartElem = unscnocIf (==_less)
{-# INLINE unsnocStartElem #-}

unscnocIf :: (Word8 -> Bool) -> ByteString -> (ByteString, Maybe Word8)
unscnocIf p bs =
  case BS.unsnoc bs of
    Just (bs', w) | p w -> (bs', Just w)
    _                   -> (bs , Nothing)
{-# INLINE unscnocIf #-}
