{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeSynonymInstances #-}

module App.Commands.CreateBlankedXml
  ( cmdCreateBlankedXml
  ) where

import Control.Lens
import Data.Generics.Product.Any
import HaskellWorks.Data.Xml.Succinct.Cursor.BlankedXml
import Options.Applicative                              hiding (columns)

import qualified App.Commands.Types   as Z
import qualified Data.ByteString.Lazy as LBS

runCreateBlankedXml :: Z.CreateBlankedXmlOptions -> IO ()
runCreateBlankedXml opt = do
  let input     = opt ^. the @"input"
  let output  = opt ^. the @"output"

  lbs <- LBS.readFile input
  let blankedXml = lbsToBlankedXml lbs
  LBS.writeFile output (LBS.fromChunks (blankedXml ^. the @1))

  return ()

optsCreateBlankedXml :: Parser Z.CreateBlankedXmlOptions
optsCreateBlankedXml = Z.CreateBlankedXmlOptions
  <$> strOption
      (   long "input"
      <>  help "Input file"
      <>  metavar "FILE"
      )
  <*> strOption
      (   long "output"
      <>  help "Blanked XML output"
      <>  metavar "FILE"
      )

cmdCreateBlankedXml :: Mod CommandFields (IO ())
cmdCreateBlankedXml = command "create-blanked-xml"  $ flip info idm $ runCreateBlankedXml <$> optsCreateBlankedXml
