{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.Data.Xml.Succinct.Cursor.BalancedParensSpec(spec) where

import qualified Data.ByteString                                  as BS
import           Data.Conduit
import           Data.Monoid                                      ((<>))
import           Data.String
import           HaskellWorks.Data.Bits.BitShown
import           HaskellWorks.Data.ByteString
import           HaskellWorks.Data.Conduit.List
import           HaskellWorks.Data.Xml.Conduit
import           HaskellWorks.Data.Xml.Conduit.Blank
import           HaskellWorks.Data.Xml.Succinct.Cursor.BlankedXml
import HaskellWorks.Data.Xml.Conduit.Blank
import           Test.Hspec

{-# ANN module ("HLint: Ignore Redundant do"        :: String) #-}

spec :: Spec
spec = describe "HaskellWorks.Data.Xml.Succinct.Cursor.BalancedParensSpec" $ do
  it "Blanking XML should work 1" $ do
    let blankedXml = BlankedXml ["<t<t>>"]
    let bp = BitShown $ BS.concat (runListConduit (blankedXmlToBalancedParens2 =$= compressWordAsBit) (getBlankedXml blankedXml))
    bp `shouldBe` fromString "11011000"
  it "Blanking XML should work 2" $ do
    let blankedXml = BlankedXml
          [ "<><><><><><><><>"
          , "<><><><><><><><>"
          ]
    let bp = BitShown $ BS.concat (runListConduit (blankedXmlToBalancedParens2 =$= compressWordAsBit) (getBlankedXml blankedXml))
    bp `shouldBe` fromString
          "1010101010101010\
          \1010101010101010"

  let unchunkedInput = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<micro_stats>\n  <metric>\n  </metric>\n  <metric></metric>\n  <metric></metric>\n  <metric></metric>\n  <metric></metric>\n</micro_stats>\n"
  let chunkedInput = chunkedBy 15 unchunkedInput
  let chunkedBlank = runListConduit blankXml chunkedInput

  let unchunkedBadInput = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<micro_stats>\n  <metric>       \n  </metric>\n  <metric></metric>\n  <metric></metric>\n  <metric></metric>\n  <metric></metric>\n</micro_stats>\n"
  let chunkedBadInput = chunkedBy 15 unchunkedBadInput
  let chunkedBadBlank = runListConduit blankXml chunkedBadInput

  it "Same input" $ do
    unchunkedInput `shouldBe` BS.concat chunkedInput

  it "Blanking XML should work 3" $ do
    let bp = BitShown $ BS.concat (runListConduit (blankedXmlToBalancedParens2 =$= compressWordAsBit) chunkedBlank)
    putStrLn $ "Good: " <> show chunkedBlank
    bp `shouldBe` fromString "11101010 10001101 01010100"

  it "Blanking XML should work 3" $ do
    let bp = BitShown $ BS.concat (runListConduit (blankedXmlToBalancedParens2 =$= compressWordAsBit) chunkedBadBlank)
    putStrLn $ "Bad: " <> show chunkedBadBlank
    bp `shouldBe` fromString "11101010 10001101 01010100"

  describe "Chunking works" $ do
    let document = "<?xml version=\"1.0\" encoding=\"UTF-8\"?><a text='value'>free</a>"
    let whole = mkBlank 4096 document
    let chunked = mkBlank 15 document

    it "should BP the same with chanks" $ do
      BS.concat chunked `shouldBe` BS.concat whole

    it "should produce same bits" $ do
      BS.concat (mkBits chunked) `shouldBe` BS.concat (mkBits whole)


mkBlank :: Int -> BS.ByteString -> [BS.ByteString]
mkBlank csize bs = runListConduit blankXml (chunkedBy csize bs)

mkBits :: [BS.ByteString] -> [BS.ByteString]
mkBits = runListConduit (blankedXmlToBalancedParens2 =$= compressWordAsBit)
