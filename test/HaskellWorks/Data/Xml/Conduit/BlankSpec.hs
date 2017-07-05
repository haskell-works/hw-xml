{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module HaskellWorks.Data.Xml.Conduit.BlankSpec (spec) where

import qualified Data.ByteString                     as BS
import           Data.Monoid
import           Data.Conduit
import           HaskellWorks.Data.Conduit.List
import           HaskellWorks.Data.Xml.Conduit.Blank
import           HaskellWorks.Data.Bits.BitShown
import HaskellWorks.Data.Xml.Conduit.Blank
import           Data.String
import           Test.Hspec
import           HaskellWorks.Data.ByteString
import           HaskellWorks.Data.Conduit.List
import           HaskellWorks.Data.Xml.Conduit
import           Test.QuickCheck
import Data.Char

{-# ANN module ("HLint: ignore Redundant do" :: String) #-}

whenBlankedXmlShouldBe :: BS.ByteString -> BS.ByteString -> Spec
whenBlankedXmlShouldBe original expected = do
  it (show original <> " when blanked json should be " <> show expected) $ do
    BS.concat (runListConduit blankXml [original]) `shouldBe` expected

repeatBS :: Int -> BS.ByteString -> BS.ByteString
repeatBS n bs | n > 0   = bs <> repeatBS (n - 1) bs
repeatBS  _ _            = BS.empty

noSpaces :: BS.ByteString -> BS.ByteString
noSpaces = BS.filter (/= fromIntegral (ord ' '))

spec :: Spec
spec = describe "HaskellWorks.Data.Xml.Conduit.BlankSpec" $ do
  describe "Can blank XML" $ do
    "<b/>"                                    `whenBlankedXmlShouldBe` "<  >"
    "<b></b>"                                 `whenBlankedXmlShouldBe` "<     >"
    "<b>text</b>"                             `whenBlankedXmlShouldBe` "<  t      >"
    "<b>  text  </b>"                         `whenBlankedXmlShouldBe` "<    t        >"
    "<b />"                                   `whenBlankedXmlShouldBe` "< ()>"
    "<foo bar='buzz' />"                      `whenBlankedXmlShouldBe` "<   (a   v      )>"
    "<foo xsd:bar='buzz' />"                  `whenBlankedXmlShouldBe` "<   (a       v      )>"
    "<foo bar=\"buzz\" />"                    `whenBlankedXmlShouldBe` "<   (a   v      )>"
    "<e a='x' b='y'/>"                        `whenBlankedXmlShouldBe` "< (a v   a v  )>"
    "<e a='x' b='y'>text</e>"                 `whenBlankedXmlShouldBe` "< (a v   a v  )t      >"
    "<e a = 'x' b = 'y' />"                   `whenBlankedXmlShouldBe` "< (a   v   a   v   )>"
    "<a x='y'><b/></a>"                       `whenBlankedXmlShouldBe` "< (a v  )<  >   >"
    "<a x='y'><b>test</b></a>"                `whenBlankedXmlShouldBe` "< (a v  )<  t      >   >"
    "<test> text <b>bold</b> </test>"         `whenBlankedXmlShouldBe` "<      t    <  t      >       >"
    "<test> text <b>bold</b> uuu</test>"      `whenBlankedXmlShouldBe` "<      t    <  t      > t        >"
    "<person fstName=\"alexey\" />"           `whenBlankedXmlShouldBe` "<      (a       v        )>"
    "<e> <!-- comment --> </e>"               `whenBlankedXmlShouldBe` "<   [              ]    >"
    "<e> <!-- a --- z --> </e>"               `whenBlankedXmlShouldBe` "<   [              ]    >"

    "<e> <!-- <b>a</b> --> </e>"              `whenBlankedXmlShouldBe` "<   [               ]    >"
    "<?xml version='1.0' encoding='UTF-8' ?>" `whenBlankedXmlShouldBe` "<    (a       v     a        v       )>"

    "<!DOCTYPE greeting [\
      \<!ELEMENT greeting (#PCDATA)>]>"       `whenBlankedXmlShouldBe` "[                   [                           ] ]"

    "<a><![CDATA[<hi>Hello,\
    \ world!</hi>]]></b>"                     `whenBlankedXmlShouldBe` "<  [                                ]   >"

    "<a><![CDATA[ [ ]]]]></b>"                `whenBlankedXmlShouldBe` "<  [               ]   >"
    "<a><c>00</c><s/></a>"                    `whenBlankedXmlShouldBe` "<  <  t    ><  >   >"
    "<a><c>0</c><s/></a>"                     `whenBlankedXmlShouldBe` "<  <  t   ><  >   >"

  it "Can blank across chunk boundaries" $ do
    let inputOriginalPrefix = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<statistics>\n  <attack>"
    let inputOriginalSuffix = "\n  </attack>\n  <attack></attack>\n  <attack></attack>\n  <attack></attack>\n  <attack></attack>\n</statistics>\n"
    let inputOriginal = inputOriginalPrefix <> inputOriginalSuffix
    let inputOriginalChunked = chunkedBy 15 inputOriginal
    let inputOriginalBlanked = runListConduit blankXml inputOriginalChunked

    forAll (choose (0, 15)) $ \(n :: Int) -> do
      let inputShifted = inputOriginalPrefix <> repeatBS n " " <> inputOriginalSuffix
      let inputShiftedChunked = chunkedBy 15 inputShifted
      let inputShiftedBlanked = runListConduit blankXml inputShiftedChunked

      noSpaces (BS.concat inputOriginalBlanked) `shouldBe` noSpaces (BS.concat inputShiftedBlanked)
