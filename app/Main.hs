{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Data.ByteString                                  as BS
import qualified Data.Vector.Storable                             as DVS
import           Data.Word
import           GHC.Conc
import           HaskellWorks.Data.Bits.BitShown
import           HaskellWorks.Data.FromByteString
import           HaskellWorks.Data.Succinct.BalancedParens.Simple
import           HaskellWorks.Data.Xml.Succinct.Cursor
import           HaskellWorks.Diagnostics.Time
import           System.Mem

readXml :: String -> IO (XmlCursor BS.ByteString (BitShown (DVS.Vector Word64)) (SimpleBalancedParens (DVS.Vector Word64)))
readXml path = do
  bs <- BS.readFile path
  print "Read file"
  !cursor <- measure (fromByteString bs :: XmlCursor BS.ByteString (BitShown (DVS.Vector Word64)) (SimpleBalancedParens (DVS.Vector Word64)))
  print "Created cursor"
  return cursor

main :: IO ()
main = do
  performGC
  !c0 <- readXml "/Users/jky/Downloads/78mbs.xml"
  !c1 <- readXml "/Users/jky/Downloads/78mbs.xml"
  !c2 <- readXml "/Users/jky/Downloads/78mbs.xml"
  !c3 <- readXml "/Users/jky/Downloads/78mbs.xml"
  !c4 <- readXml "/Users/jky/Downloads/78mbs.xml"
  !c5 <- readXml "/Users/jky/Downloads/78mbs.xml"
  !c6 <- readXml "/Users/jky/Downloads/78mbs.xml"
  !c7 <- readXml "/Users/jky/Downloads/78mbs.xml"
  !c8 <- readXml "/Users/jky/Downloads/78mbs.xml"
  !c9 <- readXml "/Users/jky/Downloads/78mbs.xml"
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
