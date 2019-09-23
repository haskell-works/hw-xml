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
import Data.Semigroup                             ((<>))
import Data.Text                                  (Text)
import HaskellWorks.Data.TreeCursor
import HaskellWorks.Data.Xml.DecodeResult
import HaskellWorks.Data.Xml.RawDecode
import HaskellWorks.Data.Xml.RawValue
import HaskellWorks.Data.Xml.Succinct.Cursor.MMap
import HaskellWorks.Data.Xml.Succinct.Index
import HaskellWorks.Data.Xml.Value
import Options.Applicative                        hiding (columns)

import qualified App.Commands.Types as Z
import qualified App.Naive          as NAIVE
import qualified App.XPath.Parser   as XPP
import qualified Data.Text          as T
import qualified System.Exit        as IO
import qualified System.IO          as IO

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
  let input   = opt ^. the @"input"
  let xpath   = opt ^. the @"xpath"
  let method  = opt ^. the @"method"

  IO.putStrLn $ "XPath: " <> show xpath

  cursorResult <- case method of
    "mmap"  -> Right <$> mmapFastCursor input
    "naive" -> Right <$> NAIVE.loadFastCursor input
    unknown -> return (Left ("Unknown method " <> show unknown))

  case cursorResult of
    Right !cursor -> do
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
    Left msg -> do
      IO.putStrLn $ "Error: " <> msg
      IO.exitFailure

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
  <*> textOption
      (   long "method"
      <>  help "Read method"
      <>  metavar "METHOD"
      )

cmdCount :: Mod CommandFields (IO ())
cmdCount = command "count"  $ flip info idm $ runCount <$> optsCount
