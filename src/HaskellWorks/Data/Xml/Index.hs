{-# LANGUAGE FlexibleInstances #-}

module HaskellWorks.Data.Xml.Index
  ( Index(..)
  , indexVersion
  , divup
  ) where

import Control.Monad
import Data.Serialize
import Data.Word
import HaskellWorks.Data.Bits.BitShown

import qualified Data.ByteString      as BS
import qualified Data.ByteString.UTF8 as BS hiding (length)
import qualified Data.Vector.Storable as DVS

indexVersion :: String
indexVersion = "1.0"

data Index = Index
  { xiVersion        :: String
  , xiInterests      :: BitShown (DVS.Vector Word64)
  , xiBalancedParens :: BitShown (DVS.Vector Word64)
  } deriving (Eq, Show)

divup :: Int -> Int -> Int
divup n d = fromIntegral (-((-sn) `div` sd)) :: Int
  where sd = fromIntegral d :: Int
        sn = fromIntegral n :: Int

putString :: Putter String
putString text = do
  let bs = BS.fromString text
  put (BS.length bs)
  let len = BS.length bs
  let padLen = 8 * (len `divup` 8) - len
  forM_ (BS.unpack bs) put
  forM_ [1..padLen] $ const (put (0 :: Word8))

getString :: Get String
getString = do
  len <- get
  let padLen = 8 * (len `divup` 8)
  bs <- BS.pack . DVS.toList . DVS.take len <$> DVS.generateM padLen (const get)
  return (BS.toString bs)

putBitShownVector :: Putter (BitShown (DVS.Vector Word64))
putBitShownVector = putVector . bitShown

getBitShownVector :: Get (BitShown (DVS.Vector Word64))
getBitShownVector = BitShown <$> getVector

putVector :: DVS.Vector Word64 -> Put
putVector v = do
  let len = DVS.length v
  put len
  DVS.forM_ v put

getVector :: Get (DVS.Vector Word64)
getVector = do
  len <- get
  DVS.generateM len (const get)

instance Serialize Index where
  put xi = do
    putString         $ xiVersion         xi
    putBitShownVector $ xiInterests       xi
    putBitShownVector $ xiBalancedParens  xi

  get = do
    version   <- getString
    ib        <- getBitShownVector
    bp        <- getBitShownVector
    return $ Index version ib bp
