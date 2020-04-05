{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeSynonymInstances #-}

module App.Commands.CreateIndex
  ( cmdCreateIndex
  ) where

import App.Options
import Control.Lens
import Control.Monad
import Data.Generics.Product.Any
import HaskellWorks.Data.Xml.Internal.ToIbBp64
import HaskellWorks.Data.Xml.Succinct.Cursor.BlankedXml
import HaskellWorks.Data.Xml.Succinct.Cursor.MMap
import Options.Applicative                              hiding (columns)

import qualified App.Commands.Types                as Z
import qualified Data.ByteString                   as BS
import qualified Data.ByteString.Lazy              as LBS
import qualified Data.Text.IO                      as TIO
import qualified HaskellWorks.Data.ByteString.Lazy as LBS
import qualified Options.Applicative               as OA
import qualified System.Exit                       as IO
import qualified System.IO                         as IO

runCreateIndex :: Z.CreateIndexOptions -> IO ()
runCreateIndex opt = do
  let input     = opt ^. the @"input"
  let ibOutput  = opt ^. the @"ibOutput"
  let bpOutput  = opt ^. the @"bpOutput"
  let method    = opt ^. the @"method"

  case method of
    "memory" -> do
      cursor <- mmapSlowCursor input

      LBS.writeFile ibOutput (LBS.toLazyByteString (cursor ^. the @"interests"      . the @1))
      LBS.writeFile bpOutput (LBS.toLazyByteString (cursor ^. the @"balancedParens" . the @1))
    "stream" -> do
      lbs <- LBS.readFile input
      let blankedXml = lbsToBlankedXml lbs
      let ibBp = toIbBp64 blankedXml

      hIbOutput <- IO.openFile ibOutput IO.WriteMode
      hBpOutput <- IO.openFile bpOutput IO.WriteMode

      forM_ ibBp $ \(ib, bp) -> do
        BS.hPut hIbOutput ib
        BS.hPut hBpOutput bp

      IO.hClose hIbOutput
      IO.hClose hBpOutput

      return ()
    unknown -> do
      TIO.hPutStrLn IO.stderr $ "Unsupported method: " <> unknown
      IO.exitFailure

optsCreateIndex :: Parser Z.CreateIndexOptions
optsCreateIndex = Z.CreateIndexOptions
  <$> strOption
      (   long "input"
      <>  help "Input file"
      <>  metavar "FILE"
      )
  <*> strOption
      (   long "ib-output"
      <>  help "Interest Bits output"
      <>  metavar "FILE"
      )
  <*> strOption
      (   long "bp-output"
      <>  help "Balanced Parens output"
      <>  metavar "FILE"
      )
  <*> textOption
      (   long "method"
      <>  help "Method"
      <>  metavar "METHOD"
      <>  OA.value "memory"
      )

cmdCreateIndex :: Mod CommandFields (IO ())
cmdCreateIndex = command "create-index"  $ flip info idm $ runCreateIndex <$> optsCreateIndex
