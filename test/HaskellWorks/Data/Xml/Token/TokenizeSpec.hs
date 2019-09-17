{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.Data.Xml.Token.TokenizeSpec (spec) where

import Data.ByteString                      as BS
import HaskellWorks.Data.Xml.Token.Tokenize
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified Data.Attoparsec.ByteString.Char8 as BC

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}

parseXmlToken' :: ByteString -> Either String (XmlToken String Double)
parseXmlToken' = BC.parseOnly parseXmlToken

spec :: Spec
spec = describe "Data.Conduit.Succinct.XmlSpec" $ do
  describe "When parsing single token at beginning of text" $ do
    it "Empty Xml should produce no bits" $ requireTest $
      parseXmlToken' "" === Left "not enough input"
    it "Xml with one space should produce whitespace token" $ requireTest $
      parseXmlToken' " " === Right XmlTokenWhitespace
    it "Xml with two spaces should produce whitespace token" $ requireTest $
      parseXmlToken' "  " === Right XmlTokenWhitespace
    it "Spaces and newlines should produce no bits" $ requireTest $
      parseXmlToken' "  \n \r \t " === Right XmlTokenWhitespace
    it "`null` at beginning should produce one bit" $ requireTest $
      parseXmlToken' "null " === Right XmlTokenNull
    it "number at beginning should produce one bit" $ requireTest $
      parseXmlToken' "1234 " === Right (XmlTokenNumber 1234)
    it "false at beginning should produce one bit" $ requireTest $
      parseXmlToken' "false " === Right (XmlTokenBoolean False)
    it "true at beginning should produce one bit" $ requireTest $
      parseXmlToken' "true " === Right (XmlTokenBoolean True)
    it "string at beginning should produce one bit" $ requireTest $
      parseXmlToken' "\"hello\" " === Right (XmlTokenString "hello")
    it "quoted string should parse" $ requireTest $
      parseXmlToken' "\"\\\"\" " === Right (XmlTokenString "\"")
    it "left brace at beginning should produce one bit" $ requireTest $
      parseXmlToken' "{ " === Right XmlTokenBraceL
    it "right brace at beginning should produce one bit" $ requireTest $
      parseXmlToken' "} " === Right XmlTokenBraceR
    it "left bracket at beginning should produce one bit" $ requireTest $
      parseXmlToken' "[ " === Right XmlTokenBracketL
    it "right bracket at beginning should produce one bit" $ requireTest $
      parseXmlToken' "] " === Right XmlTokenBracketR
    it "right bracket at beginning should produce one bit" $ requireTest $
      parseXmlToken' ": " === Right XmlTokenColon
    it "right bracket at beginning should produce one bit" $ requireTest $
      parseXmlToken' ", " === Right XmlTokenComma
