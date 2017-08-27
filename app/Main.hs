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
  !c0 <- readXml "сorpus/105mb.xml"
  !c1 <- readXml "сorpus/105mb.xml"
  !c2 <- readXml "сorpus/105mb.xml"
  !c3 <- readXml "сorpus/105mb.xml"
  !c4 <- readXml "сorpus/105mb.xml"
  !c5 <- readXml "сorpus/105mb.xml"
  !c6 <- readXml "сorpus/105mb.xml"
  !c7 <- readXml "сorpus/105mb.xml"
  !c8 <- readXml "сorpus/105mb.xml"
  !c9 <- readXml "сorpus/105mb.xml"
  print "Returned from readXml"
  performGC
  threadDelay 100000000
  print c0
  print c1
  print c2
  print c3
  print c4
  print c5
  print c6
  print c7
  print c8
  print c9
