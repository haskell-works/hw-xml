{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.Xml.Conduit.BlankSpec (spec) where

import Data.Char
import Data.Monoid
import HaskellWorks.Data.ByteString
import HaskellWorks.Data.Conduit.List
import HaskellWorks.Data.Xml.Conduit.Blank
import Test.Hspec
import Test.QuickCheck

import qualified Data.ByteString as BS

{-# ANN module ("HLint: ignore Redundant do" :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

whenBlankedXmlShouldBe :: BS.ByteString -> BS.ByteString -> Spec
whenBlankedXmlShouldBe original expected = do
  it (show original <> " when blanked json should be " <> show expected) $ do
    BS.concat (runListConduit blankXml [original]) `shouldBe` expected

repeatBS :: Int -> BS.ByteString -> BS.ByteString
repeatBS n bs | n > 0   = bs <> repeatBS (n - 1) bs
repeatBS  _ _ = BS.empty

noSpaces :: BS.ByteString -> BS.ByteString
noSpaces = BS.filter (/= fromIntegral (ord ' '))

data Annotated a b = Annotated a b deriving Show

instance Eq a => Eq (Annotated a b) where
  (Annotated a _) == (Annotated b _) = a == b

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

  it "Can blank across chunk boundaries with basic tags" $ do
    let inputOriginalPrefix = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<statistics>\n  <attack>"
    let inputOriginalSuffix = "\n  </attack>\n  <attack></attack>\n  <attack></attack>\n  <attack></attack>\n  <attack></attack>\n</statistics>\n"
    let inputOriginal = inputOriginalPrefix <> inputOriginalSuffix
    let inputOriginalChunked = chunkedBy 16 inputOriginal
    let inputOriginalBlanked = runListConduit blankXml inputOriginalChunked

    forAll (choose (0, 16)) $ \(n :: Int) -> do
      let inputShifted = inputOriginalPrefix <> repeatBS n " " <> inputOriginalSuffix
      let inputShiftedChunked = chunkedBy 16 inputShifted
      let inputShiftedBlanked = runListConduit blankXml inputShiftedChunked

      noSpaces (BS.concat inputShiftedBlanked) `shouldBe` noSpaces (BS.concat inputOriginalBlanked)
  it "Can blank across chunk boundaries with auto-close tags" $ do
    let inputOriginalPrefix = "<?xml version=\"1.0\" encoding=\"UTF-8\"?><statistics><attack>"
    let inputOriginalSuffix = "<inner/></attack><attack></attack></statistics>\n"
    let inputOriginal = inputOriginalPrefix <> inputOriginalSuffix
    let inputOriginalChunked = chunkedBy 16 inputOriginal
    let inputOriginalBlanked = runListConduit blankXml inputOriginalChunked

    forAll (choose (0, 16)) $ \(n :: Int) -> do
      let inputShifted = inputOriginalPrefix <> repeatBS n " " <> inputOriginalSuffix
      let inputShiftedChunked = chunkedBy 16 inputShifted
      let inputShiftedBlanked = runListConduit blankXml inputShiftedChunked

      -- putStrLn $ show (BS.concat inputShiftedBlanked) <> " vs " <> show (BS.concat inputOriginalBlanked)
      let actual    = Annotated (noSpaces (BS.concat inputShiftedBlanked )) (inputShiftedBlanked, n)
      let expected  = Annotated (noSpaces (BS.concat inputOriginalBlanked)) (inputOriginalBlanked, n)

      actual `shouldBe` expected
  it "Can blank across chunk boundaries with auto-close tags" $ do
    let inputOriginalPrefix = "<?xml version=\"1.0\" encoding=\"UTF-8\"?><statistics><attack>"
    let inputOriginalSuffix = "<inner/></attack><attack></attack></statistics>\n"
    let inputOriginal = inputOriginalPrefix <> inputOriginalSuffix
    let inputOriginalChunked = chunkedBy 16 inputOriginal
    let inputOriginalBlanked = runListConduit blankXml inputOriginalChunked

    let n = 15
    let inputShifted = inputOriginalPrefix <> repeatBS n " " <> inputOriginalSuffix
    let inputShiftedChunked = chunkedBy 16 inputShifted
    let inputShiftedBlanked = runListConduit blankXml inputShiftedChunked

    -- putStrLn $ show (BS.concat inputShiftedBlanked) <> " vs " <> show (BS.concat inputOriginalBlanked)
    let actual    = Annotated (noSpaces (BS.concat inputShiftedBlanked )) (inputShiftedBlanked, n)
    let expected  = Annotated (noSpaces (BS.concat inputOriginalBlanked)) (inputOriginalBlanked, n)

    actual `shouldBe` expected
