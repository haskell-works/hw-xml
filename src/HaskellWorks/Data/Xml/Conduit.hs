{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module HaskellWorks.Data.Xml.Conduit
  ( blankedXmlToInterestBits
  , byteStringToBits
  , compressWordAsBit
  , interestingWord8s
  , isInterestingWord8
  ) where

import Data.ByteString                as BS
import Data.Word
import Data.Word8
import HaskellWorks.Data.AtIndex      ((!!!))
import HaskellWorks.Data.Bits.BitWise
import Prelude                        as P

import qualified Data.Bits            as BITS
import qualified Data.Vector.Storable as DVS

interestingWord8s :: DVS.Vector Word8
interestingWord8s = DVS.constructN 256 go
  where go :: DVS.Vector Word8 -> Word8
        go v = if     w == _bracketleft
                  ||  w == _braceleft
                  ||  w == _parenleft
                  ||  w == _bracketleft
                  ||  w == _less
                  ||  w == _a
                  ||  w == _v
                  ||  w == _t
              then 1
              else 0
          where w :: Word8
                w = fromIntegral (DVS.length v)
{-# NOINLINE interestingWord8s #-}

isInterestingWord8 :: Word8 -> Word8
isInterestingWord8 b = fromIntegral (interestingWord8s !!! fromIntegral b)
{-# INLINABLE isInterestingWord8 #-}

blankedXmlToInterestBits :: [BS.ByteString] -> [BS.ByteString]
blankedXmlToInterestBits = blankedXmlToInterestBits' ""

blankedXmlToInterestBits' :: BS.ByteString -> [BS.ByteString] -> [BS.ByteString]
blankedXmlToInterestBits' rs is = case is of
  (bs:bss) -> do
    let cs = if BS.length rs /= 0 then BS.concat [rs, bs] else bs
    let lencs = BS.length cs
    let q = lencs `quot` 8
    let (ds, es) = BS.splitAt (q * 8) cs
    let (fs, _) = BS.unfoldrN q gen ds
    fs:blankedXmlToInterestBits' es bss
  [] -> do
    let lenrs = BS.length rs
    let q = lenrs + 7 `quot` 8
    [fst (BS.unfoldrN q gen rs)]
  where gen :: ByteString -> Maybe (Word8, ByteString)
        gen as = if BS.length as == 0
          then Nothing
          else Just ( BS.foldr' (\b m -> isInterestingWord8 b .|. (m .<. 1)) 0 (BS.take 8 as)
                    , BS.drop 8 as
                    )

repartitionMod8 :: BS.ByteString -> BS.ByteString -> (BS.ByteString, BS.ByteString)
repartitionMod8 aBS bBS = (BS.take cLen abBS, BS.drop cLen abBS)
  where abBS = BS.concat [aBS, bBS]
        abLen = BS.length abBS
        cLen = (abLen `div` 8) * 8

compressWordAsBit :: [BS.ByteString] -> [BS.ByteString]
compressWordAsBit = compressWordAsBit' BS.empty

compressWordAsBit' :: BS.ByteString -> [BS.ByteString] -> [BS.ByteString]
compressWordAsBit' aBS iBS = case iBS of
  (bBS:bBSs) -> do
    let (cBS, dBS) = repartitionMod8 aBS bBS
    let (cs, _) = BS.unfoldrN (BS.length cBS + 7 `div` 8) gen cBS
    cs:compressWordAsBit' dBS bBSs
  [] -> do
    let (cs, _) = BS.unfoldrN (BS.length aBS + 7 `div` 8) gen aBS
    [cs]
  where gen :: ByteString -> Maybe (Word8, ByteString)
        gen xs = if BS.length xs == 0
          then Nothing
          else Just ( BS.foldr' (\b m -> ((b .&. 1) .|. (m .<. 1))) 0 (BS.take 8 xs)
                    , BS.drop 8 xs
                    )

yieldBitsOfWord8 :: Word8 -> [Bool]
yieldBitsOfWord8 w =
  [ (w .&. BITS.bit 0) /= 0
  , (w .&. BITS.bit 1) /= 0
  , (w .&. BITS.bit 2) /= 0
  , (w .&. BITS.bit 3) /= 0
  , (w .&. BITS.bit 4) /= 0
  , (w .&. BITS.bit 5) /= 0
  , (w .&. BITS.bit 6) /= 0
  , (w .&. BITS.bit 7) /= 0
  ]

yieldBitsofWord8s :: [Word8] -> [Bool]
yieldBitsofWord8s = P.foldr ((++) . yieldBitsOfWord8) []

byteStringToBits :: [BS.ByteString] -> [Bool]
byteStringToBits is = case is of
  (bs:bss) -> yieldBitsofWord8s (BS.unpack bs) ++ byteStringToBits bss
  []       -> []
