{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}


module HaskellWorks.Data.Xml.Succinct.Cursor.InterestBitsSpec(spec) where

import qualified Data.ByteString                                    as BS
import           Data.String
import qualified Data.Vector.Storable                               as DVS
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
    (interestBitsOf ""               :: BitShown (DVS.Vector Word8)) `shouldBe` fromString ""
    (interestBitsOf "  \n \r \t "    :: BitShown (DVS.Vector Word8)) `shouldBe` fromString "00000000"
    (interestBitsOf "<el atr"        :: BitShown (DVS.Vector Word8)) `shouldBe` fromString "10011000"
    (interestBitsOf "[alse "         :: BitShown (DVS.Vector Word8)) `shouldBe` fromString "10000000"
    (interestBitsOf "(rue "          :: BitShown (DVS.Vector Word8)) `shouldBe` fromString "10000000"
    (interestBitsOf "<e><c></e>"     :: BitShown (DVS.Vector Word8)) `shouldBe` fromString "10010000 00000000"
    (interestBitsOf " <e p='a'/> "   :: BitShown (DVS.Vector Word8)) `shouldBe` fromString "01011010 00000000"
    (interestBitsOf " <!-- u -->"    :: BitShown (DVS.Vector Word8)) `shouldBe` fromString "01000000 00000000"
    (interestBitsOf "<![CDATA[ x"    :: BitShown (DVS.Vector Word8)) `shouldBe` fromString "10000000 00000000"
