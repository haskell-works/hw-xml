{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Word
import GHC.Conc
import HaskellWorks.Data.Bits.BitShown
import HaskellWorks.Data.FromByteString
import HaskellWorks.Data.BalancedParens.Simple
import HaskellWorks.Data.Xml.Succinct.Cursor
import System.Mem

import qualified Data.ByteString      as BS
import qualified Data.Vector.Storable as DVS

readXml :: String -> IO (XmlCursor BS.ByteString (BitShown (DVS.Vector Word64)) (SimpleBalancedParens (DVS.Vector Word64)))
readXml path = do
  bs <- BS.readFile path
  print "Read file"
  let !cursor = fromByteString bs :: XmlCursor BS.ByteString (BitShown (DVS.Vector Word64)) (SimpleBalancedParens (DVS.Vector Word64))
  print "Created cursor"
  return cursor

main :: IO ()
main = do
  performGC
  !c0 <- readXml "data/catalog.xml"
  print "Returned from readXml"
  performGC
  threadDelay 100000000
  print c0
