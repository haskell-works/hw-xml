{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.Data.Xml.Conduit.BlankSpec (spec) where

import qualified Data.ByteString                      as BS
import           HaskellWorks.Data.Xml.Conduit.Blank
import           HaskellWorks.Data.Conduit.List
import           Test.Hspec

{-# ANN module ("HLint: ignore Redundant do" :: String) #-}

whenBlankedXmlShouldBe :: BS.ByteString -> BS.ByteString -> Spec
whenBlankedXmlShouldBe original expected = do
  it (show original ++ " when blanked json should be " ++ show expected) $ do
    BS.concat (runListConduit blankXml [original]) `shouldBe` expected

spec :: Spec
spec = describe "HaskellWorks.Data.Xml.Conduit.BlankSpec" $ do
  describe "Can blank json" $ do
    "\"\""                                `whenBlankedXmlShouldBe` "()"
    "\"\\\\\""                            `whenBlankedXmlShouldBe` "(  )"
    "\"\\\\\\\""                          `whenBlankedXmlShouldBe` "(    "
    "\" \\\\\\\""                         `whenBlankedXmlShouldBe` "(     "
    "\" \\n\\\\\""                        `whenBlankedXmlShouldBe` "(     )"
    ""                                    `whenBlankedXmlShouldBe` ""
    "\"\""                                `whenBlankedXmlShouldBe` "()"
    "\" \""                               `whenBlankedXmlShouldBe` "( )"
    "\" a \""                             `whenBlankedXmlShouldBe` "(   )"
    " \"a \" x"                           `whenBlankedXmlShouldBe` " (  ) x"
    " \"a\"b\"c\"d"                       `whenBlankedXmlShouldBe` " ( )b( )d"
    ""                                    `whenBlankedXmlShouldBe` ""
    "1"                                   `whenBlankedXmlShouldBe` "1"
    "11"                                  `whenBlankedXmlShouldBe` "10"
    "00"                                  `whenBlankedXmlShouldBe` "10"
    "00"                                  `whenBlankedXmlShouldBe` "10"
    "-0.12e+34"                           `whenBlankedXmlShouldBe` "100000000"
    "10.12E-34 "                          `whenBlankedXmlShouldBe` "100000000 "
    "10.12E-34 12"                        `whenBlankedXmlShouldBe` "100000000 10"
    " 10.12E-34 -1"                       `whenBlankedXmlShouldBe` " 100000000 10"
    ""                                    `whenBlankedXmlShouldBe` ""
    "a"                                   `whenBlankedXmlShouldBe` "a"
    "z"                                   `whenBlankedXmlShouldBe` "z"
    " Aaa "                               `whenBlankedXmlShouldBe` " A__ "
    " Za def "                            `whenBlankedXmlShouldBe` " Z_ d__ "
    ""                                    `whenBlankedXmlShouldBe` ""
    " { \"ff\": 1.0, [\"\", true], null}" `whenBlankedXmlShouldBe` " { (  ): 100, [(), t___], n___}"
