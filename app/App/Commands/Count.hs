{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeSynonymInstances #-}

module App.Commands.Count
  ( cmdCount
  ) where

import App.Options
import Control.Lens
import Control.Monad
import Data.Generics.Product.Any
import Data.Semigroup                                   ((<>))
import Data.Text                                        (Text)
import Data.Word
import Foreign.ForeignPtr
import HaskellWorks.Data.BalancedParens.RangeMin2
import HaskellWorks.Data.BalancedParens.Simple
import HaskellWorks.Data.Bits.BitShown
import HaskellWorks.Data.RankSelect.CsPoppy1
import HaskellWorks.Data.TreeCursor
import HaskellWorks.Data.Vector.Storable
import HaskellWorks.Data.Xml.DecodeResult
import HaskellWorks.Data.Xml.RawDecode
import HaskellWorks.Data.Xml.RawValue
import HaskellWorks.Data.Xml.Succinct.Cursor
import HaskellWorks.Data.Xml.Succinct.Cursor.BlankedXml
import HaskellWorks.Data.Xml.Succinct.Index
import HaskellWorks.Data.Xml.Value
import Options.Applicative                              hiding (columns)

import qualified App.Commands.Types                      as Z
import qualified App.XPath.Parser                        as XPP
import qualified Data.ByteString                         as BS
import qualified Data.ByteString.Internal                as BSI
import qualified Data.Text                               as T
import qualified Data.Vector.Storable                    as DVS
import qualified HaskellWorks.Data.Xml.Internal.ToIbBp64 as I
import qualified System.IO                               as IO
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

-- | Read an XML file into memory and return a Count-optimised cursor initialised
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

-- | Document model.  This does not need to be able to completely represent all
-- the data in the XML document.  In fact, having a smaller model may improve
-- Count performance.
data Plant = Plant
  { common :: String
  , price  :: String
  } deriving (Eq, Show)

newtype Catalog = Catalog
  { plants :: [Plant]
  } deriving (Eq, Show)

tags :: Value -> String -> [Value]
tags xml@(XmlElement n _ _) elemName = if n == elemName
  then [xml]
  else []
tags _ _ = []

kids :: Value -> [Value]
kids (XmlElement _ _ cs) = cs
kids _                   = []

countAtPath :: [Text] -> Value -> DecodeResult Int
countAtPath []  _   = return 0
countAtPath [t] xml = return (length (tags xml (T.unpack t)))
countAtPath (t:ts) xml = do
  counts <- forM (tags xml (T.unpack t) >>= kids) $ countAtPath ts
  return (sum counts)

runCount :: Z.CountOptions -> IO ()
runCount opt = do
  let input = opt ^. the @"input"
  let xpath = opt ^. the @"xpath"

  IO.putStrLn $ "XPath: " <> show xpath

  -- Read XML into memory as a Count-optimised cursor
  !cursor <- readFastCursor input

  -- Skip the XML declaration to get to the root element cursor
  case nextSibling cursor of
    Just rootCursor -> do
      -- Get the root raw XML value at the root element cursor
      let rootValue = rawValueAt (xmlIndexAt rootCursor)
      -- Show what we have at this cursor
      putStrLn $ "Raw value: " <> take 100 (show rootValue)
      -- Decode the raw XML value
      case countAtPath (xpath ^. the @"path") (rawDecode rootValue) of
        DecodeOk count   -> putStrLn $ "Count: " <> show count
        DecodeFailed msg -> putStrLn $ "Error: " <> show msg
    Nothing -> do
      putStrLn "Could not read XML"
      return ()

optsCount :: Parser Z.CountOptions
optsCount = Z.CountOptions
  <$> strOption
      (   long "input"
      <>  help "Input file"
      <>  metavar "FILE"
      )
  <*> optionParser XPP.path
      (   long "xpath"
      <>  help "XPath expression"
      <>  metavar "XPATH"
      )

cmdCount :: Mod CommandFields (IO ())
cmdCount = command "count"  $ flip info idm $ runCount <$> optsCount
