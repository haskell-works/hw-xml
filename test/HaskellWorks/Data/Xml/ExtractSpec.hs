{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.Data.Xml.ExtractSpec (spec) where

import           HaskellWorks.Data.Xml.Extract
import           HaskellWorks.Data.Xml.Type
import           Test.Hspec

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}

spec :: Spec
spec = describe "HaskellWorks.Data.Xml.ExtractSpec" $ do
  describe "When extracting type and string" $ do
    it "should work for empty string only" $
      extractXmlSnippet "\"\"" `shouldBe` Just (XmlTypeString, "\"\"")
    it "should work for empty string followed by space" $
      extractXmlSnippet "\"\" " `shouldBe` Just (XmlTypeString, "\"\"")
    it "should work for empty string followed by double quote" $
      extractXmlSnippet "\"\"\"" `shouldBe` Just (XmlTypeString, "\"\"")
    it "should work for empty string followed by comma" $
      extractXmlSnippet "\"\"," `shouldBe` Just (XmlTypeString, "\"\"")
    it "should work for string with escaped double quotes" $
      extractXmlSnippet "\"\\\"\"," `shouldBe` Just (XmlTypeString, "\"\\\"\"")
    it "should work for string with escaped double quotes and spaces" $
      extractXmlSnippet "\" \\\" \"," `shouldBe` Just (XmlTypeString, "\" \\\" \"")
    it "should work for null" $
      extractXmlSnippet "null, 1" `shouldBe` Just (XmlTypeNull, "n")
    it "should work for true" $
      extractXmlSnippet "true, 1" `shouldBe` Just (XmlTypeBool, "t")
    it "should work for false" $
      extractXmlSnippet "false, 1" `shouldBe` Just (XmlTypeBool, "f")
    it "should work for number only" $
      extractXmlSnippet "1.0e-2" `shouldBe` Just (XmlTypeNumber, "1.0e-2")
    it "should work for number followed by space" $
      extractXmlSnippet "-123.02e+2 " `shouldBe` Just (XmlTypeNumber, "-123.02e+2")
    it "should work for number followed by space" $
      extractXmlSnippet "[1, 2, 3] " `shouldBe` Just (XmlTypeArray, "[")
    it "should work for number followed by space" $
      extractXmlSnippet "{\"field\": 1}" `shouldBe` Just (XmlTypeObject, "{")
