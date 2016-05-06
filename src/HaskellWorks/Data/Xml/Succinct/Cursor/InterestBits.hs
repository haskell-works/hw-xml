{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module HaskellWorks.Data.Xml.Succinct.Cursor.InterestBits
  ( XmlInterestBits(..)
  , getXmlInterestBits
  ) where

import           Control.Applicative
import qualified Data.ByteString                                       as BS
import           Data.ByteString.Internal
import qualified Data.Vector.Storable                                  as DVS
import           Data.Word
import           HaskellWorks.Data.Bits.BitShown
import           HaskellWorks.Data.Conduit.Json
import           HaskellWorks.Data.Conduit.List
import           HaskellWorks.Data.FromByteString
import           HaskellWorks.Data.Xml.Succinct.Cursor.BlankedXml
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Poppy512

newtype XmlInterestBits a = XmlInterestBits a

getXmlInterestBits :: XmlInterestBits a -> a
getXmlInterestBits (XmlInterestBits a) = a

blankedXmlBssToInterestBitsBs :: [ByteString] -> ByteString
blankedXmlBssToInterestBitsBs bss = BS.concat $ runListConduit blankedJsonToInterestBits bss

genInterest :: ByteString -> Maybe (Word8, ByteString)
genInterest = BS.uncons

genInterestForever :: ByteString -> Maybe (Word8, ByteString)
genInterestForever bs = BS.uncons bs <|> Just (0, bs)

instance FromBlankedXml (XmlInterestBits (BitShown [Bool])) where
  fromBlankedXml = XmlInterestBits . fromByteString . BS.concat . runListConduit blankedJsonToInterestBits . getBlankedXml

instance FromBlankedXml (XmlInterestBits (BitShown BS.ByteString)) where
  fromBlankedXml = XmlInterestBits . BitShown . BS.unfoldr genInterest . blankedXmlBssToInterestBitsBs . getBlankedXml

instance FromBlankedXml (XmlInterestBits (BitShown (DVS.Vector Word8))) where
  fromBlankedXml = XmlInterestBits . BitShown . DVS.unfoldr genInterest . blankedXmlBssToInterestBitsBs . getBlankedXml

instance FromBlankedXml (XmlInterestBits (BitShown (DVS.Vector Word16))) where
  fromBlankedXml bj = XmlInterestBits (BitShown (DVS.unsafeCast (DVS.unfoldrN newLen genInterestForever interestBS)))
    where interestBS    = blankedXmlBssToInterestBitsBs (getBlankedXml bj)
          newLen        = (BS.length interestBS + 1) `div` 2 * 2

instance FromBlankedXml (XmlInterestBits (BitShown (DVS.Vector Word32))) where
  fromBlankedXml bj = XmlInterestBits (BitShown (DVS.unsafeCast (DVS.unfoldrN newLen genInterestForever interestBS)))
    where interestBS    = blankedXmlBssToInterestBitsBs (getBlankedXml bj)
          newLen        = (BS.length interestBS + 3) `div` 4 * 4

instance FromBlankedXml (XmlInterestBits (BitShown (DVS.Vector Word64))) where
  fromBlankedXml bj    = XmlInterestBits (BitShown (DVS.unsafeCast (DVS.unfoldrN newLen genInterestForever interestBS)))
    where interestBS    = blankedXmlBssToInterestBitsBs (getBlankedXml bj)
          newLen        = (BS.length interestBS + 7) `div` 8 * 8

instance FromBlankedXml (XmlInterestBits Poppy512) where
  fromBlankedXml = XmlInterestBits . makePoppy512 . bitShown . getXmlInterestBits . fromBlankedXml
