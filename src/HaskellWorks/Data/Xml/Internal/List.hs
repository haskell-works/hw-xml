{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module HaskellWorks.Data.Xml.Internal.List
  ( blankedXmlToInterestBits
  , compressWordAsBit
  , compressWord8sAsBit
  ) where

import Control.Monad
import Control.Monad.ST                          (ST)
import Data.ByteString                           (ByteString)
import Data.Word
import HaskellWorks.Data.AtIndex
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.ByteString
import HaskellWorks.Data.Positioning
import HaskellWorks.Data.Vector.AsVector8
import HaskellWorks.Data.Xml.Internal.ByteString
import HaskellWorks.Data.Xml.Internal.Tables
import Prelude

import qualified Data.ByteString              as BS
import qualified Data.Vector.Storable         as DVS
import qualified Data.Vector.Storable.Mutable as DVSM

blankedXmlToInterestBits :: [ByteString] -> [ByteString]
blankedXmlToInterestBits = blankedXmlToInterestBits' ""

blankedXmlToInterestBits' :: ByteString -> [ByteString] -> [ByteString]
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

compressWordAsBit :: [ByteString] -> [ByteString]
compressWordAsBit = compressWordAsBit' BS.empty

compressWordAsBit' :: ByteString -> [ByteString] -> [ByteString]
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

compressWord8sAsBit :: [ByteString] -> [ByteString]
compressWord8sAsBit = compressWord8sAsBit' BS.empty

compressWord8sAsBit' :: ByteString -> [ByteString] -> [ByteString]
compressWord8sAsBit' as [] = if BS.length as == 0
  then []
  else [rs]
  where aLen  = BS.length as
        rsLen = (aLen + 7) `div` 8
        abLen = rsLen * 8
        bLen  = abLen - aLen
        bs    = BS.replicate bLen 0
        rs    = mkRs rsLen as bs
compressWord8sAsBit' as (bs:bss) = rs:compressWord8sAsBit' ds bss
  where aLen  = BS.length as
        bLen  = BS.length bs
        abLen = aLen + bLen
        rsLen = abLen `div` 8
        dsLen = abLen - rsLen * 8
        rs    = mkRs rsLen as bs
        cs    = if dsLen > bLen then as <> bs else bs
        ds    = BS.drop (BS.length cs - dsLen) cs

mkRs :: Int -> ByteString -> ByteString -> ByteString
mkRs n as bs = toByteString (mkRs' n as bs)

mkRs' :: Int -> ByteString -> ByteString -> DVS.Vector Word8
mkRs' size as bs = DVS.create $ do
  mv <- DVSM.new size
  (n, w) <- writeRs 0 0 (asVector8 as) 0 mv
  _      <- writeRs n 0 (asVector8 bs) w mv
  return mv

writeRs :: Count -> Int -> DVS.Vector Word8 -> Word8 -> DVSM.MVector s Word8 -> ST s (Count, Word8)
writeRs bitOffset ui u w mv = if ui < uLen
  then do
    let index = bitOffset .>. 3
    let bitValue = (u !!! fromIntegral ui) .&. 1
    let nw :: Word8 = w .|. (bitValue .<. bitOffset)

    when (bitOffset .&. 7 == 7) $ DVSM.write mv (fromIntegral index) w
    writeRs (bitOffset + 1) (ui + 1) u nw mv
  else return (bitOffset, 0)
  where uLen = DVS.length u
