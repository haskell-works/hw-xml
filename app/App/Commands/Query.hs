{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeSynonymInstances #-}

module App.Commands.Query
  ( cmdQuery
  ) where

import Control.Lens
import Data.Foldable
import Data.Generics.Product.Any
import Data.Maybe
import Data.Semigroup                                   ((<>))
import Data.Word
import Foreign.ForeignPtr
import HaskellWorks.Data.BalancedParens.RangeMin2
import HaskellWorks.Data.BalancedParens.Simple
import HaskellWorks.Data.Bits.BitShown
import HaskellWorks.Data.RankSelect.CsPoppy1
import HaskellWorks.Data.TreeCursor
import HaskellWorks.Data.Vector.Storable
import HaskellWorks.Data.Xml.Decode
import HaskellWorks.Data.Xml.DecodeResult
import HaskellWorks.Data.Xml.RawDecode
import HaskellWorks.Data.Xml.RawValue
import HaskellWorks.Data.Xml.Succinct.Cursor
import HaskellWorks.Data.Xml.Succinct.Cursor.BlankedXml
import HaskellWorks.Data.Xml.Succinct.Index
import HaskellWorks.Data.Xml.Value
import Options.Applicative                              hiding (columns)

import qualified App.Commands.Types                      as Z
import qualified Data.ByteString                         as BS
import qualified Data.ByteString.Internal                as BSI
import qualified Data.Vector.Storable                    as DVS
import qualified HaskellWorks.Data.Xml.Internal.ToIbBp64 as I
import qualified System.IO.MMap                          as IO

type RawCursor = XmlCursor BS.ByteString (BitShown (DVS.Vector Word64)) (SimpleBalancedParens (DVS.Vector Word64))
type FastCursor = XmlCursor BS.ByteString CsPoppy1 (RangeMin2 CsPoppy1)

-- | Read an XML file into memory and return a raw cursor initialised to the
-- start of the XML document.
mmapRawCursor :: String -> IO RawCursor
mmapRawCursor filePath = do
  (fptr :: ForeignPtr Word8, offset, size) <- IO.mmapFileForeignPtr filePath IO.ReadOnly Nothing
  let !bs = BSI.fromForeignPtr (castForeignPtr fptr) offset size
  let blankedXml = bsToBlankedXml bs
  let (ib, bp) = construct64UnzipN (fromIntegral size) (I.toIbBp64 blankedXml)
  let !cursor = XmlCursor
        { cursorText      = bs
        , interests       = BitShown ib
        , balancedParens  = SimpleBalancedParens bp
        , cursorRank      = 1
        }

  return cursor

-- | Read an XML file into memory and return a query-optimised cursor initialised
-- to the start of the XML document.
readFastCursor :: String -> IO FastCursor
readFastCursor filename = do
  -- Load the XML file into memory as a raw cursor.
  -- The raw XML data is `text`, and `ib` and `bp` are the indexes.
  -- `ib` and `bp` can be persisted to an index file for later use to avoid
  -- re-parsing the file.
  XmlCursor !text (BitShown !ib) (SimpleBalancedParens !bp) _ <- mmapRawCursor filename
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

runQuery :: Z.QueryOptions -> IO ()
runQuery opt = do
  let input = opt ^. the @"input"

  -- Read XML into memory as a query-optimised cursor
  !cursor <- readFastCursor input

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

optsQuery :: Parser Z.QueryOptions
optsQuery = Z.QueryOptions
  <$> strOption
      (   long "input"
      <>  help "Input file"
      <>  metavar "FILE"
      )

cmdQuery :: Mod CommandFields (IO ())
cmdQuery = command "query"  $ flip info idm $ runQuery <$> optsQuery
