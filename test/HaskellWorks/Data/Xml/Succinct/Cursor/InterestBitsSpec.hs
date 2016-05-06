{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}


module HaskellWorks.Data.Xml.Succinct.Cursor.InterestBitsSpec(spec) where

import qualified Data.ByteString                                     as BS
import           Data.String
import qualified Data.Vector.Storable                                as DVS
import           Data.Word
import           HaskellWorks.Data.Bits.BitShown
import           HaskellWorks.Data.FromByteString
import           HaskellWorks.Data.Xml.Succinct.Cursor.BlankedXml
import           HaskellWorks.Data.Xml.Succinct.Cursor.InterestBits
import           Test.Hspec

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}

interestBitsOf :: FromBlankedXml (XmlInterestBits a) => BS.ByteString -> a
interestBitsOf = getXmlInterestBits . fromBlankedXml . fromByteString

spec :: Spec
spec = describe "HaskellWorks.Data.Xml.Succinct.Cursor.InterestBitsSpec" $ do
  it "Evaluating interest bits" $ do
    (interestBitsOf ""            :: BitShown (DVS.Vector Word8)) `shouldBe` fromString ""
    (interestBitsOf "  \n \r \t " :: BitShown (DVS.Vector Word8)) `shouldBe` fromString "00000000"
    (interestBitsOf "1234 "       :: BitShown (DVS.Vector Word8)) `shouldBe` fromString "10000000"
    (interestBitsOf "false "      :: BitShown (DVS.Vector Word8)) `shouldBe` fromString "10000000"
    (interestBitsOf "true "       :: BitShown (DVS.Vector Word8)) `shouldBe` fromString "10000000"
    (interestBitsOf "\"hello\" "  :: BitShown (DVS.Vector Word8)) `shouldBe` fromString "10000000"
    (interestBitsOf "\"\\\"\" "   :: BitShown (DVS.Vector Word8)) `shouldBe` fromString "10000000"
    (interestBitsOf "{ "          :: BitShown (DVS.Vector Word8)) `shouldBe` fromString "10000000"
    (interestBitsOf "} "          :: BitShown (DVS.Vector Word8)) `shouldBe` fromString "00000000"
    (interestBitsOf "[ "          :: BitShown (DVS.Vector Word8)) `shouldBe` fromString "10000000"
    (interestBitsOf "] "          :: BitShown (DVS.Vector Word8)) `shouldBe` fromString "00000000"
    (interestBitsOf ": "          :: BitShown (DVS.Vector Word8)) `shouldBe` fromString "00000000"
    (interestBitsOf ", "          :: BitShown (DVS.Vector Word8)) `shouldBe` fromString "00000000"
    (interestBitsOf "{{}}"        :: BitShown (DVS.Vector Word8)) `shouldBe` fromString "11000000"
    (interestBitsOf " { { } } "   :: BitShown (DVS.Vector Word8)) `shouldBe` fromString "01010000 00000000"
