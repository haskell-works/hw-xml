{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.Data.Xml.Token.TokenizeSpec (spec) where

import qualified Data.Attoparsec.ByteString.Char8      as BC
import           Data.ByteString                       as BS
import           HaskellWorks.Data.Xml.Token.Tokenize
import           Test.Hspec

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}

parseXmlToken' :: ByteString -> Either String (XmlToken String Double)
parseXmlToken' = BC.parseOnly parseXmlToken

spec :: Spec
spec = describe "Data.Conduit.Succinct.XmlSpec" $ do
  describe "When parsing single token at beginning of text" $ do
    it "Empty Xml should produce no bits" $
      parseXmlToken' "" `shouldBe` Left "not enough input"
    it "Xml with one space should produce whitespace token" $
      parseXmlToken' " " `shouldBe` Right XmlTokenWhitespace
    it "Xml with two spaces should produce whitespace token" $
      parseXmlToken' "  " `shouldBe` Right XmlTokenWhitespace
    it "Spaces and newlines should produce no bits" $
      parseXmlToken' "  \n \r \t " `shouldBe` Right XmlTokenWhitespace
    it "`null` at beginning should produce one bit" $
      parseXmlToken' "null " `shouldBe` Right XmlTokenNull
    it "number at beginning should produce one bit" $
      parseXmlToken' "1234 " `shouldBe` Right (XmlTokenNumber 1234)
    it "false at beginning should produce one bit" $
      parseXmlToken' "false " `shouldBe` Right (XmlTokenBoolean False)
    it "true at beginning should produce one bit" $
      parseXmlToken' "true " `shouldBe` Right (XmlTokenBoolean True)
    it "string at beginning should produce one bit" $
      parseXmlToken' "\"hello\" " `shouldBe` Right (XmlTokenString "hello")
    it "quoted string should parse" $
      parseXmlToken' "\"\\\"\" " `shouldBe` Right (XmlTokenString "\"")
    it "left brace at beginning should produce one bit" $
      parseXmlToken' "{ " `shouldBe` Right XmlTokenBraceL
    it "right brace at beginning should produce one bit" $
      parseXmlToken' "} " `shouldBe` Right XmlTokenBraceR
    it "left bracket at beginning should produce one bit" $
      parseXmlToken' "[ " `shouldBe` Right XmlTokenBracketL
    it "right bracket at beginning should produce one bit" $
      parseXmlToken' "] " `shouldBe` Right XmlTokenBracketR
    it "right bracket at beginning should produce one bit" $
      parseXmlToken' ": " `shouldBe` Right XmlTokenColon
    it "right bracket at beginning should produce one bit" $
      parseXmlToken' ", " `shouldBe` Right XmlTokenComma
