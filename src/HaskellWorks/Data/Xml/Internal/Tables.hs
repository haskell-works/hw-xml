module HaskellWorks.Data.Xml.Internal.Tables
  ( isInterestingWord8
  ) where

import Data.Word
import Data.Word8
import HaskellWorks.Data.AtIndex ((!!!))
import Prelude                   as P

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
