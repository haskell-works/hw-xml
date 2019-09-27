{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeSynonymInstances #-}

module App.Commands.CreateBpIndex
  ( cmdCreateBpIndex
  ) where

import Control.Lens
import Data.Generics.Product.Any
import Data.Semigroup                                   ((<>))
import HaskellWorks.Data.Xml.Internal.ToIbBp64
import HaskellWorks.Data.Xml.Succinct.Cursor.BlankedXml
import Options.Applicative                              hiding (columns)

import qualified App.Commands.Types   as Z
import qualified Data.ByteString.Lazy as LBS

runCreateBpIndex :: Z.CreateBpIndexOptions -> IO ()
runCreateBpIndex opt = do
  let input     = opt ^. the @"input"
  let output  = opt ^. the @"output"

  lbs <- LBS.readFile input
  let blankedXml = lbsToBlankedXml lbs
  let ib = toBalancedParens64' blankedXml
  LBS.writeFile output (LBS.fromChunks ib)

  return ()

optsCreateBpIndex :: Parser Z.CreateBpIndexOptions
optsCreateBpIndex = Z.CreateBpIndexOptions
  <$> strOption
      (   long "input"
      <>  help "Input file"
      <>  metavar "FILE"
      )
  <*> strOption
      (   long "output"
      <>  help "Balanced parens output"
      <>  metavar "FILE"
      )

cmdCreateBpIndex :: Mod CommandFields (IO ())
cmdCreateBpIndex = command "create-bp-index"  $ flip info idm $ runCreateBpIndex <$> optsCreateBpIndex
