module HaskellWorks.Data.Xml.Internal.BalancedParens
  ( blankedXmlToBalancedParens
  ) where

import Data.ByteString (ByteString)
import Data.Word
import Data.Word8

import qualified Data.ByteString as BS

data MiniBP = MiniN | MiniT | MiniF | MiniTF

blankedXmlToBalancedParens :: [ByteString] -> [ByteString]
blankedXmlToBalancedParens is = case is of
  (bs:bss) -> do
    let (cs, _) = BS.unfoldrN (BS.length bs * 2) gen (Nothing, bs)
    cs:blankedXmlToBalancedParens bss
  [] -> []
  where gen :: (Maybe Bool, ByteString) -> Maybe (Word8, (Maybe Bool, ByteString))
        gen (Just True  , bs) = Just (0xFF, (Nothing, bs))
        gen (Just False , bs) = Just (0x00, (Nothing, bs))
        gen (Nothing    , bs) = case BS.uncons bs of
          Just (c, cs) -> case balancedParensOf c of
            MiniN  -> gen         (Nothing    , cs)
            MiniT  -> Just (0xFF, (Nothing    , cs))
            MiniF  -> Just (0x00, (Nothing    , cs))
            MiniTF -> Just (0xFF, (Just False , cs))
          Nothing   -> Nothing

balancedParensOf :: Word8 -> MiniBP
balancedParensOf c = case c of
    d | d == _less         -> MiniT
    d | d == _greater      -> MiniF
    d | d == _bracketleft  -> MiniT
    d | d == _bracketright -> MiniF
    d | d == _parenleft    -> MiniT
    d | d == _parenright   -> MiniF
    d | d == _t            -> MiniTF
    d | d == _a            -> MiniTF
    d | d == _v            -> MiniTF
    _                      -> MiniN

