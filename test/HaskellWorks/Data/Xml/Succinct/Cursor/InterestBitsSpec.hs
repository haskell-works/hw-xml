{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.Data.Xml.Succinct.Cursor.InterestBitsSpec(spec) where

import Data.String
import Data.Word
import HaskellWorks.Data.Bits.BitShown
import HaskellWorks.Data.FromByteString
import HaskellWorks.Data.Xml.Succinct.Cursor.BlankedXml
import HaskellWorks.Data.Xml.Succinct.Cursor.InterestBits
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified Data.ByteString      as BS
import qualified Data.Vector.Storable as DVS

{- HLINT ignore "Redundant do"        -}

interestBitsOf :: FromBlankedXml (XmlInterestBits a) => BS.ByteString -> a
interestBitsOf = getXmlInterestBits . fromBlankedXml . bsToBlankedXml

spec :: Spec
spec = describe "HaskellWorks.Data.Xml.Succinct.Cursor.InterestBitsSpec" $ do
  it "Evaluating interest bits" $ requireTest $ do
    (interestBitsOf ""               :: BitShown (DVS.Vector Word8)) === fromString ""
    (interestBitsOf "  \n \r \t "    :: BitShown (DVS.Vector Word8)) === fromString "00000000"
    (interestBitsOf "<el atr"        :: BitShown (DVS.Vector Word8)) === fromString "10011000"
    (interestBitsOf "[alse "         :: BitShown (DVS.Vector Word8)) === fromString "10000000"
    (interestBitsOf "(rue "          :: BitShown (DVS.Vector Word8)) === fromString "10000000"
    (interestBitsOf "<e><c></e>"     :: BitShown (DVS.Vector Word8)) === fromString "10010000 00000000"
    (interestBitsOf " <e p='a'/> "   :: BitShown (DVS.Vector Word8)) === fromString "01011010 00000000"
    (interestBitsOf " <!-- u -->"    :: BitShown (DVS.Vector Word8)) === fromString "01000000 00000000"
    (interestBitsOf "<![CDATA[ x"    :: BitShown (DVS.Vector Word8)) === fromString "10000000 00000000"
  it "Can build interest bits across boundaries" $ requireTest $ do
    let blanked =
          [ "<    (a "
          , "      v "
          , "    a   "
          , "     v  "
          , "    )> "
          , "<  <     "
          , ">   >"
          ]
    annotate $ "Blanked: " <> show blanked
    let ib :: XmlInterestBits (BitShown (DVS.Vector Word8))
        ib = XmlInterestBits (getXmlInterestBits (fromBlankedXml (BlankedXml blanked)))
    let moo :: [BS.ByteString]
        moo = blankedXmlToInterestBits blanked -- :: XmlInterestBits (BitShown (DVS.Vector Word8))
    annotate $ "Moo:       " <> show (BitShown . BS.unpack <$> moo)
    let actual = getXmlInterestBits ib :: BitShown (DVS.Vector Word8)
    let expected = fromString "10000110 00000010 00001000 00000100 00000001 00100000 00000000"

    actual === expected
