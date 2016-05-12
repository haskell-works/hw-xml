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

data ByteStringP = BSP Word8 ByteString | EmptyBSP

blankXml :: MonadThrow m => Conduit BS.ByteString m BS.ByteString
blankXml = blankXml' Nothing InXml

blankXml' :: MonadThrow m => Maybe Word8 -> BlankState -> Conduit BS.ByteString m BS.ByteString
blankXml' lastChar lastState = do
  mbs <- await
  case prefix lastChar mbs of
    Just bsp -> do
      let (safe, next) = unsnocStartElem bsp
      let (!cs, Just (!nextState, _)) = unfoldrN (lenBSP safe) blankByteString (lastState, safe)
      yield cs
      blankXml' next nextState
    Nothing -> return ()
  where
    blankByteString :: (BlankState, ByteStringP) -> Maybe (Word8, (BlankState, ByteStringP))
    blankByteString (_, EmptyBSP) = Nothing
    blankByteString (InXml, bs) = case bs of
      BSP !c !cs | c == _less         -> Just (c           , (InElement , toBSP cs))
      BSP !c !cs                      -> Just (c           , (InXml     , toBSP cs))
    blankByteString (InElement, bs) = case bs of
      BSP !c !cs | c == _greater      -> Just (c           , (InXml     , toBSP cs))
      BSP !c !cs | isSpace c          -> Just (_parenleft  , (InAttrList, toBSP cs))
      BSP !c !cs                      -> Just (c           , (InElement , toBSP cs))
    blankByteString (InAttrList, bs) = case bs of
      BSP !c !cs | c == _greater      -> Just (_parenright , (InXml     , toBSP cs))
      BSP !c !cs | c == _slash        -> Just (_parenright , (InElement , toBSP cs))
      BSP !c !cs | isNameChar c       -> Just (_parenleft  , (InIdent   , toBSP cs))
      BSP !c !cs | c == _equal        -> Just (c           , (InAttrList, toBSP cs))
      BSP !c !cs | isQuote c          -> Just (_parenleft  , (InString c, toBSP cs))
      BSP !c !cs                      -> Just (c           , (InAttrList, toBSP cs))
    blankByteString (InIdent, bs) = case bs of
      BSP !c !cs | isNameChar c       -> Just (c           , (InIdent   , toBSP cs))
      BSP !c !cs | isSpace c          -> Just (_parenright , (InAttrList, toBSP cs))
      BSP !c !cs | c == _equal        -> Just (_parenright , (InAttrList, toBSP cs))
      BSP _  !cs                      -> Just (_parenright , (InAttrList, toBSP cs))
    blankByteString (InString q, bs) = case bs of
      BSP !c !cs | c == q             -> Just (_parenright , (InAttrList, toBSP cs))
      BSP !c !cs                      -> Just (c           , (InString q, toBSP cs))
    blankByteString (Test, bs) = case bs of
      BSP !c !cs                      -> Just (c           , (Test      , toBSP cs))

prefix :: Maybe Word8 -> Maybe ByteString -> Maybe (ByteStringP)
prefix (Just s) (Just bs) = Just $ BSP s bs
prefix (Just s) (Nothing) = Just $ BSP s BS.empty
prefix Nothing  (Just bs) = (\(!c, !cs) -> BSP c cs) <$> BS.uncons bs
prefix Nothing  Nothing   = Nothing

toBSP :: ByteString -> ByteStringP
toBSP bs = case BS.uncons bs of
  Just (!c, !cs) -> BSP c cs
  Nothing        -> EmptyBSP

lenBSP :: ByteStringP -> Int
lenBSP (BSP _ bs) = BS.length bs + 1
lenBSP EmptyBSP = 0

headIs :: (Word8 -> Bool) -> ByteString -> Maybe Bool
headIs p bs = case BS.uncons bs of
  Just (!c, _) -> Just (p c)
  Nothing      -> Nothing
{-# INLINE headIs #-}

unsnocStartElem :: ByteStringP -> (ByteStringP, Maybe Word8)
unsnocStartElem = unscnocIf (==_less)
{-# INLINE unsnocStartElem #-}

unscnocIf :: (Word8 -> Bool) -> ByteStringP -> (ByteStringP, Maybe Word8)
unscnocIf _ EmptyBSP = (EmptyBSP, Nothing)
unscnocIf p (BSP !c !bs) =
  case BS.unsnoc bs of
    Just (bs', w) | p w -> (BSP c bs', Just w)
    _                   -> (BSP c bs , Nothing)
{-# INLINE unscnocIf #-}
