{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Semigroup                                ((<>))
import Data.Word
import HaskellWorks.Data.BalancedParens.RangeMinMax2
import HaskellWorks.Data.BalancedParens.Simple
import HaskellWorks.Data.Bits.BitShown
import HaskellWorks.Data.FromByteString
import HaskellWorks.Data.RankSelect.CsPoppy
import HaskellWorks.Data.TreeCursor
import HaskellWorks.Data.Xml.RawValue
import HaskellWorks.Data.Xml.Succinct.Cursor
import HaskellWorks.Data.Xml.Succinct.Index

import qualified Data.ByteString      as BS
import qualified Data.Vector.Storable as DVS

type RawCursor = XmlCursor BS.ByteString (BitShown (DVS.Vector Word64)) (SimpleBalancedParens (DVS.Vector Word64))
type CsCursor = XmlCursor BS.ByteString CsPoppy (RangeMinMax2 CsPoppy)

readRawCursor :: String -> IO RawCursor
readRawCursor path = do
  !bs <- BS.readFile path
  let !cursor = fromByteString bs :: RawCursor
  return cursor

readCsCursor :: String -> IO CsCursor
readCsCursor filename = do
  XmlCursor !text (BitShown !ib) (SimpleBalancedParens !bp) _ <- readRawCursor filename
  let !bpCsPoppy = makeCsPoppy bp
  let !rangeMinMax = mkRangeMinMax2 bpCsPoppy
  let !ibCsPoppy = makeCsPoppy ib
  return $ XmlCursor text ibCsPoppy rangeMinMax 1

main :: IO ()
main = do
  !cursor <- readCsCursor "data/catalog.xml"
  case nextSibling cursor of
    Just rootCursor -> do
      let rootValue = rawValueAt (xmlIndexAt rootCursor)
      putStrLn $ "Raw value: " <> take 100 (show rootValue)
    Nothing -> do
      putStrLn "Could not read XML"
      return ()
