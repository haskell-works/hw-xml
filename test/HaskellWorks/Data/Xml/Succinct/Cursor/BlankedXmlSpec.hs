{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.Data.Xml.Succinct.Cursor.BlankedXmlSpec
  ( spec
  ) where

import HaskellWorks.Data.Xml.Succinct.Cursor.BlankedXml
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}

spec :: Spec
spec = describe "HaskellWorks.Data.Xml.Succinct.Cursor.BlankedXmlSpec" $ do
  describe "Blanking XML should work" $ do
    it "on strict bytestrings" $ requireTest $ do
      let input       = "<attack><instances/></attack>"
      let expected    = "<       <          >        >"
      let blankedXml  = bsToBlankedXml input

      mconcat (unblankedXml blankedXml) === expected

    it "on lazy bytestrings" $ requireTest $ do
      let input       = "<attack><instances/></attack>"
      let expected    = "<       <          >        >"
      let blankedXml  = lbsToBlankedXml input

      mconcat (unblankedXml blankedXml) === expected
