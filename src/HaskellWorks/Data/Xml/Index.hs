{-# LANGUAGE FlexibleInstances #-}

module HaskellWorks.Data.Xml.Index
  ( Index(..)
  , indexVersion
  ) where

import Data.Serialize
import Data.Word
import HaskellWorks.Data.Bits.BitShown

import qualified Data.Vector.Storable as DVS

indexVersion :: String
indexVersion = "1.0"

data Index = Index
  { xiVersion        :: String
  , xiInterests      :: BitShown (DVS.Vector Word64)
  , xiBalancedParens :: BitShown (DVS.Vector Word64)
  } deriving (Eq, Show)

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
    put               $ xiVersion         xi
    putBitShownVector $ xiInterests       xi
    putBitShownVector $ xiBalancedParens  xi

  get = do
    version   <- get
    ib        <- getBitShownVector
    bp        <- getBitShownVector
    return $ Index version ib bp
