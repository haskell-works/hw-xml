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

type Attempt        = Int
type ExpectedCount  = Int
type ExpectedChar      = Word8
data BlankState
  = InXml
  | InTag | InAttrList | InCloseTag | InClose
  | InBang Int
  | InString ExpectedChar | InText
  | InMeta
  | InCdataTag | InCdata Int
  | InRemTag | InRem Int
  | InIdent

data ByteStringP = BSP Word8 ByteString | EmptyBSP

blankXml :: MonadThrow m => Conduit BS.ByteString m BS.ByteString
blankXml = blankXml' Nothing InXml

blankXml' :: MonadThrow m => Maybe Word8 -> BlankState -> Conduit BS.ByteString m BS.ByteString
blankXml' lastChar lastState = do
  mbs <- await
  case prefix lastChar mbs of
    Just bsp -> do
      let (safe, next) = unsnocUndecided bsp
      let (!cs, Just (!nextState, _)) = unfoldrN (lenBSP safe) blankByteString (lastState, safe)
      yield cs
      blankXml' next nextState
    Nothing -> return ()
  where
    blankByteString :: (BlankState, ByteStringP) -> Maybe (Word8, (BlankState, ByteStringP))
    blankByteString (_, EmptyBSP) = Nothing
    blankByteString (InXml, bs) = case bs of
      BSP !c !cs | isMetaStart c cs     -> Just (_bracketleft , (InMeta    , toBSP cs))
      BSP !c !cs | isEndTag c cs        -> Just (_space       , (InCloseTag, toBSP cs))
      BSP !c !cs | c == _less           -> Just (_less        , (InTag     , toBSP cs))
      BSP !c !cs | isSpace c            -> Just (_space       , (InXml     , toBSP cs))
      BSP !c !cs                        -> Just (_parenleft   , (InText    , toBSP cs))
    blankByteString (InTag, bs) = case bs of
      BSP !c !cs | isSpace c            -> Just (_parenleft   , (InAttrList, toBSP cs))
      BSP !c !cs | isTagClose c cs      -> Just (_space       , (InClose   , toBSP cs))
      BSP !c !cs | c == _greater        -> Just (_space       , (InXml     , toBSP cs))
      BSP !c !cs                        -> Just (_space       , (InTag     , toBSP cs))
    blankByteString (InCloseTag, bs) = case bs of
      BSP !c !cs | c == _greater        -> Just (_greater     , (InXml     , toBSP cs))
      BSP !c !cs                        -> Just (_space       , (InCloseTag, toBSP cs))
    blankByteString (InAttrList, bs) = case bs of
      BSP !c !cs | c == _greater        -> Just (_parenright  , (InXml     , toBSP cs))
      BSP !c !cs | isTagClose c cs      -> Just (_parenright  , (InClose   , toBSP cs))
      BSP !c !cs | isNameChar c         -> Just (_parenleft   , (InIdent   , toBSP cs))
      BSP !c !cs | isQuote c            -> Just (_parenleft   , (InString c, toBSP cs))
      BSP !c !cs                        -> Just (_space       , (InAttrList, toBSP cs))
    blankByteString (InClose, bs) = case bs of
      BSP _ !cs                         -> Just (_greater     , (InXml     , toBSP cs))
    blankByteString (InIdent, bs) = case bs of
      BSP !c !cs | isNameChar c         -> Just (_space       , (InIdent   , toBSP cs))
      BSP !c !cs | isSpace c            -> Just (_parenright  , (InAttrList, toBSP cs))
      BSP !c !cs | c == _equal          -> Just (_parenright  , (InAttrList, toBSP cs))
      BSP _  !cs                        -> Just (_parenright  , (InAttrList, toBSP cs))
    blankByteString (InString q, bs) = case bs of
      BSP !c !cs | c == q               -> Just (_parenright  , (InAttrList, toBSP cs))
      BSP !c !cs                        -> Just (_space       , (InString q, toBSP cs))
    blankByteString (InText, bs) = case bs of
      BSP !c !cs | headIs (== _less) cs -> Just (_parenright  , (InXml     , toBSP cs))
      BSP !c !cs                        -> Just (_space       , (InText    , toBSP cs))

    blankByteString (InMeta, bs) = case bs of
      BSP !c !cs | c == _exclam            -> Just (_space       , (InMeta       , toBSP cs))
      BSP !c !cs | c == _hyphen            -> Just (_space       , (InRem 0      , toBSP cs))
      BSP !c !cs | c == _bracketleft       -> Just (_space       , (InCdataTag   , toBSP cs))
      BSP !c !cs | c == _greater           -> Just (_bracketright, (InXml        , toBSP cs))
      BSP _  !cs                           -> Just (_space       , (InBang 1     , toBSP cs))
    blankByteString (InCdataTag, bs) = case bs of
      BSP !c !cs | c == _bracketleft       -> Just (_parenleft   , (InCdata 0    , toBSP cs))
      BSP _  !cs                           -> Just (_space       , (InCdataTag   , toBSP cs))
    blankByteString (InCdata n, bs) = case bs of
      BSP !c !cs | c == _greater && n >= 2 -> Just (_bracketright, (InXml        , toBSP cs))
      BSP !c !cs | isCdataEnd c cs && n>0  -> Just (_parenright  , (InCdata (n+1), toBSP cs))
      BSP !c !cs | c == _bracketright      -> Just (_space       , (InCdata (n+1), toBSP cs))
      BSP !c !cs                           -> Just (_space       , (InCdata 0    , toBSP cs))

    blankByteString (InRem n, bs) = case bs of
      BSP !c !cs | c == _greater && n >= 2 -> Just (_bracketright, (InXml        , toBSP cs))
      BSP !c !cs | c == _hyphen            -> Just (_space       , (InRem (n+1)  , toBSP cs))
      BSP !c !cs                           -> Just (_space       , (InRem 0      , toBSP cs))

    blankByteString (InBang n, bs) = case bs of
      BSP !c !cs | c == _less              -> Just (_bracketleft , (InBang (n+1) , toBSP cs))
      BSP !c !cs | c == _greater && n == 1 -> Just (_bracketright, (InXml        , toBSP cs))
      BSP !c !cs | c == _greater           -> Just (_bracketright, (InBang (n-1) , toBSP cs))
      BSP _  !cs                           -> Just (_space       , (InBang n     , toBSP cs))

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

isEndTag :: Word8 -> ByteString -> Bool
isEndTag c cs = c == _less && headIs (== _slash) cs

isStartTag :: Word8 -> ByteString -> Bool
isStartTag c cs = c == _less && headIs isNameStartChar cs

isTagClose :: Word8 -> ByteString -> Bool
isTagClose c cs =
  (c == _slash || c == _question) && headIs (== _greater) cs

isMetaStart :: Word8 -> ByteString -> Bool
isMetaStart c cs = c == _less && headIs (== _exclam) cs

isCdataEnd :: Word8 -> ByteString -> Bool
isCdataEnd c cs = c == _bracketright && headIs (== _greater) cs

isCommentClose :: Word8 -> ByteString -> Bool
isCommentClose c cs = c == _hyphen && headIs (== _greater) cs

unsnocUndecided :: ByteStringP -> (ByteStringP, Maybe Word8)
unsnocUndecided = unscnocIf (\w -> w ==_less           -- <elem> or </elem>?
                                || w == _slash         -- <elem /> or not?
                                || w == _hyphen        -- closing comment or just - ?
                                || w == _bracketright) -- closing CDATA or just data?
{-# INLINE unsnocUndecided #-}

headIs :: (Word8 -> Bool) -> ByteString -> Bool
headIs p bs = case BS.uncons bs of
  Just (!c, _) -> p c
  Nothing      -> False
{-# INLINE headIs #-}

unscnocIf :: (Word8 -> Bool) -> ByteStringP -> (ByteStringP, Maybe Word8)
unscnocIf _ EmptyBSP = (EmptyBSP, Nothing)
unscnocIf p (BSP !c !bs) =
  case BS.unsnoc bs of
    Just (bs', w) | p w -> (BSP c bs', Just w)
    _                   -> (BSP c bs , Nothing)
{-# INLINE unscnocIf #-}
