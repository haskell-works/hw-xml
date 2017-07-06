{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.Data.Xml.Succinct.Cursor.InterestBitsSpec(spec) where

import Data.String
import Data.Word
import HaskellWorks.Data.Bits.BitShown
import HaskellWorks.Data.ByteString
import HaskellWorks.Data.Conduit.List
import HaskellWorks.Data.FromByteString
import HaskellWorks.Data.Xml.Conduit.Blank
import HaskellWorks.Data.Xml.Succinct.Cursor.BlankedXml
import HaskellWorks.Data.Xml.Succinct.Cursor.InterestBits
import Test.Hspec

import qualified Data.ByteString      as BS
import qualified Data.Vector.Storable as DVS

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}

interestBitsOf :: FromBlankedXml (XmlInterestBits a) => BS.ByteString -> a
interestBitsOf = getXmlInterestBits . fromBlankedXml . fromByteString

spec :: Spec
spec = describe "HaskellWorks.Data.Xml.Succinct.Cursor.InterestBitsSpec" $ do
  it "Evaluating interest bits" $ do
    (interestBitsOf ""               :: BitShown (DVS.Vector Word8)) `shouldBe` fromString ""
    (interestBitsOf "  \n \r \t "    :: BitShown (DVS.Vector Word8)) `shouldBe` fromString "00000000"
    (interestBitsOf "<el atr"        :: BitShown (DVS.Vector Word8)) `shouldBe` fromString "10011000"
    (interestBitsOf "[alse "         :: BitShown (DVS.Vector Word8)) `shouldBe` fromString "10000000"
    (interestBitsOf "(rue "          :: BitShown (DVS.Vector Word8)) `shouldBe` fromString "10000000"
    (interestBitsOf "<e><c></e>"     :: BitShown (DVS.Vector Word8)) `shouldBe` fromString "10010000 00000000"
    (interestBitsOf " <e p='a'/> "   :: BitShown (DVS.Vector Word8)) `shouldBe` fromString "01011010 00000000"
    (interestBitsOf " <!-- u -->"    :: BitShown (DVS.Vector Word8)) `shouldBe` fromString "01000000 00000000"
    (interestBitsOf "<![CDATA[ x"    :: BitShown (DVS.Vector Word8)) `shouldBe` fromString "10000000 00000000"
  it "Can build interest bits across boundaries" $ do
    let text = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n\
                \<a><b></b></a>"
    let blanked = BlankedXml (runListConduit blankXml (chunkedBy 8 text))
    let ib = XmlInterestBits (getXmlInterestBits (fromBlankedXml blanked))
    let actual = getXmlInterestBits ib :: BitShown (DVS.Vector Word8)
    let expected = fromString "10000110 00000010 00001000 00000100 00000010 01000000 00000000"

    actual `shouldBe` expected
