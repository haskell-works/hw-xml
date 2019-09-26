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

import Control.Lens
import Control.Monad
import Data.Generics.Product.Any
import Data.Semigroup                             ((<>))
import HaskellWorks.Data.Xml.Succinct.Cursor.MMap
import Options.Applicative                        hiding (columns)

import qualified App.Commands.Types                as Z
import qualified Data.ByteString.Lazy              as LBS
import qualified HaskellWorks.Data.ByteString.Lazy as LBS

runCreateIndex :: Z.CreateIndexOptions -> IO ()
runCreateIndex opt = do
  let input         = opt ^. the @"input"
  let maybeIbOutput = opt ^. the @"ibOutput"
  let maybeBpOutput = opt ^. the @"bpOutput"

  cursor <- mmapSlowCursor input

  forM_ maybeIbOutput $ flip LBS.writeFile (LBS.toLazyByteString (cursor ^. the @"interests"      . the @1))
  forM_ maybeBpOutput $ flip LBS.writeFile (LBS.toLazyByteString (cursor ^. the @"balancedParens" . the @1))

  return ()

optsCreateIndex :: Parser Z.CreateIndexOptions
optsCreateIndex = Z.CreateIndexOptions
  <$> strOption
      (   long "input"
      <>  help "Input file"
      <>  metavar "FILE"
      )
  <*> optional
      ( strOption
        (   long "ib-output"
        <>  help "Interest Bits output"
        <>  metavar "FILE"
        )
      )
  <*> optional
      ( strOption
        (   long "bp-output"
        <>  help "Balanced Parens output"
        <>  metavar "FILE"
        )
      )

cmdCreateIndex :: Mod CommandFields (IO ())
cmdCreateIndex = command "create-index"  $ flip info idm $ runCreateIndex <$> optsCreateIndex
