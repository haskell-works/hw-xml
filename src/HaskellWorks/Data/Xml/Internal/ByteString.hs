module HaskellWorks.Data.Xml.Internal.ByteString
  ( repartitionMod8
  , realign
  , pad
  , padAligned
  ) where

import Data.ByteString (ByteString)
import Data.Word

import qualified Data.ByteString as BS

repartitionMod8 :: ByteString -> ByteString -> (ByteString, ByteString)
repartitionMod8 aBS bBS = (BS.take cLen abBS, BS.drop cLen abBS)
  where abBS = BS.concat [aBS, bBS]
        abLen = BS.length abBS
        cLen = (abLen `div` 8) * 8
{-# INLINE repartitionMod8 #-}

pad :: Int -> Word8 -> ByteString -> ByteString
pad n w bs = if n > bsLen
  then bs <> BS.replicate (n - bsLen) w
  else bs
  where bsLen = BS.length bs

padAligned :: Int -> Word8 -> ByteString -> ByteString
padAligned n w bs = pad (((BS.length bs + (n - 1)) `div` n) * n) w bs

realign :: Int -> [ByteString] -> [(ByteString, ByteString)]
realign = realign' BS.empty

realign' :: ByteString -> Int -> [ByteString] -> [(ByteString, ByteString)]
realign' os alignment bss = case BS.length os of
  osLen ->
    case bss of -- osLen = 9
    cs:css -> case BS.length cs of
      csLen -> case osLen + csLen of -- csLen = 7
        oscsLen -> if oscsLen < alignment -- oscsLen = 8
          then (BS.empty, BS.empty):realign' (os <> cs) alignment css
          else case alignment - oscsLen of
            need -> case BS.take need cs of -- need = 0
              ds -> case BS.drop need cs of
                es -> case BS.length es of
                  esLen -> case (esLen `div` alignment) * alignment of
                    esBoundary -> case BS.take esBoundary es of
                      fs -> case BS.drop esBoundary es of
                        gh -> (os <> ds, fs):realign' gh alignment css
    [] -> [(padAligned alignment 0 os, BS.empty)]
