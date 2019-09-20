{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeSynonymInstances #-}

module App.Commands.Demo
  ( cmdDemo
  ) where

import Data.Foldable
import Data.Maybe
import Data.Semigroup                             ((<>))
import Data.Word
import HaskellWorks.Data.BalancedParens.RangeMin2
import HaskellWorks.Data.BalancedParens.Simple
import HaskellWorks.Data.Bits.BitShown
import HaskellWorks.Data.FromByteString
import HaskellWorks.Data.RankSelect.CsPoppy1
import HaskellWorks.Data.TreeCursor
import HaskellWorks.Data.Xml.Decode
import HaskellWorks.Data.Xml.DecodeResult
import HaskellWorks.Data.Xml.RawDecode
import HaskellWorks.Data.Xml.RawValue
import HaskellWorks.Data.Xml.Succinct.Cursor
import HaskellWorks.Data.Xml.Succinct.Index
import HaskellWorks.Data.Xml.Value
import Options.Applicative                        hiding (columns)

import qualified App.Commands.Types   as Z
import qualified Data.ByteString      as BS
import qualified Data.Vector.Storable as DVS

type RawCursor = XmlCursor BS.ByteString (BitShown (DVS.Vector Word64)) (SimpleBalancedParens (DVS.Vector Word64))
type FastCursor = XmlCursor BS.ByteString CsPoppy1 (RangeMin2 CsPoppy1)

-- | Read an XML file into memory and return a raw cursor initialised to the
-- start of the XML document.
readRawCursor :: String -> IO RawCursor
readRawCursor path = do
  !bs <- BS.readFile path
  let !cursor = fromByteString bs :: RawCursor
  return cursor

-- | Read an XML file into memory and return a query-optimised cursor initialised
-- to the start of the XML document.
readFastCursor :: String -> IO FastCursor
readFastCursor filename = do
  -- Load the XML file into memory as a raw cursor.
  -- The raw XML data is `text`, and `ib` and `bp` are the indexes.
  -- `ib` and `bp` can be persisted to an index file for later use to avoid
  -- re-parsing the file.
  XmlCursor !text (BitShown !ib) (SimpleBalancedParens !bp) _ <- readRawCursor filename
  let !bpCsPoppy = makeCsPoppy bp
  let !rangeMinMax = mkRangeMin2 bpCsPoppy
  let !ibCsPoppy = makeCsPoppy ib
  return $ XmlCursor text ibCsPoppy rangeMinMax 1

-- | Parse the text of an XML node.
class ParseText a where
  parseText :: Value -> DecodeResult a

instance ParseText String where
  parseText (XmlText text)      = DecodeOk text
  parseText (XmlCData text)     = DecodeOk text
  parseText (XmlElement _ _ cs) = DecodeOk $ concat $ concat $ toList . parseText <$> cs
  parseText _                   = DecodeOk ""

-- | Convert a decode result to a maybe
decodeResultToMaybe :: DecodeResult a -> Maybe a
decodeResultToMaybe (DecodeOk a) = Just a
decodeResultToMaybe _            = Nothing

-- | Document model.  This does not need to be able to completely represent all
-- the data in the XML document.  In fact, having a smaller model may improve
-- query performance.
data Plant = Plant
  { common :: String
  , price  :: String
  } deriving (Eq, Show)

newtype Catalog = Catalog
  { plants :: [Plant]
  } deriving (Eq, Show)

-- | Decode plant element
decodePlant :: Value -> DecodeResult Plant
decodePlant xml = do
  aCommon <- xml /> "common"  >>= parseText
  aPrice  <- xml /> "price"   >>= parseText
  return $ Plant aCommon aPrice

-- | Decode catalog element
decodeCatalog :: Value -> DecodeResult Catalog
decodeCatalog xml = do
  aPlantXmls <- xml />> "plant"
  let aPlants = catMaybes (decodeResultToMaybe . decodePlant <$> aPlantXmls)
  return $ Catalog aPlants

runDemo :: Z.DemoOptions -> IO ()
runDemo _ = do
  -- Read XML into memory as a query-optimised cursor
  !cursor <- readFastCursor "data/catalog.xml"
  -- Skip the XML declaration to get to the root element cursor
  case nextSibling cursor of
    Just rootCursor -> do
      -- Get the root raw XML value at the root element cursor
      let rootValue = rawValueAt (xmlIndexAt rootCursor)
      -- Show what we have at this cursor
      putStrLn $ "Raw value: " <> take 100 (show rootValue)
      -- Decode the raw XML value
      case decodeCatalog (rawDecode rootValue) of
        DecodeOk catalog -> putStrLn $ "Catalog: " <> show catalog
        DecodeFailed msg -> putStrLn $ "Error: " <> show msg
    Nothing -> do
      putStrLn "Could not read XML"
      return ()

optsDemo :: Parser Z.DemoOptions
optsDemo = pure Z.DemoOptions

cmdDemo :: Mod CommandFields (IO ())
cmdDemo = command "demo"  $ flip info idm $ runDemo <$> optsDemo
