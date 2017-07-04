{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.Data.Xml.Succinct.Cursor.BalancedParensSpec(spec) where

import qualified Data.ByteString                                    as BS
import           Data.Conduit
import           Data.String
import           HaskellWorks.Data.Bits.BitShown
import           HaskellWorks.Data.Conduit.List
import           HaskellWorks.Data.Xml.Conduit
import           HaskellWorks.Data.Xml.Succinct.Cursor.BlankedXml
import           Test.Hspec

{-# ANN module ("HLint: Ignore Redundant do"        :: String) #-}

spec :: Spec
spec = describe "HaskellWorks.Data.Xml.Succinct.Cursor.BalancedParensSpec" $ do
  it "Blanking XML should not contain strange characters 2" $ do
    let blankedXml = BlankedXml ["<t<t>>"]
    let bp = BitShown $ BS.concat (runListConduit (blankedXmlToBalancedParens2 =$= compressWordAsBit) (getBlankedXml blankedXml))
    bp `shouldBe` fromString "11011000"
