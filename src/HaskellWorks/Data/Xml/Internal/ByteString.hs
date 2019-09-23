module HaskellWorks.Data.Xml.Internal.ByteString
  ( repartitionMod8
  ) where

import Data.ByteString (ByteString)

import qualified Data.ByteString as BS

repartitionMod8 :: ByteString -> ByteString -> (ByteString, ByteString)
repartitionMod8 aBS bBS = BS.splitAt cLen abBS --(BS.take cLen abBS, BS.drop cLen abBS)
  where abBS = aBS <> bBS
        cLen = (BS.length abBS `div` 8) * 8
{-# INLINE repartitionMod8 #-}
