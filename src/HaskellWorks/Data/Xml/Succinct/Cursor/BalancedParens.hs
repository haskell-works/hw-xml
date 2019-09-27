{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module HaskellWorks.Data.Xml.Succinct.Cursor.BalancedParens
  ( XmlBalancedParens(..)
  , getXmlBalancedParens
  ) where

import Control.Applicative
import Data.Word
import HaskellWorks.Data.BalancedParens                 as BP
import HaskellWorks.Data.Xml.Internal.BalancedParens
import HaskellWorks.Data.Xml.Internal.List
import HaskellWorks.Data.Xml.Succinct.Cursor.BlankedXml

import qualified Data.ByteString      as BS
import qualified Data.Vector.Storable as DVS

newtype XmlBalancedParens a = XmlBalancedParens a

getXmlBalancedParens :: XmlBalancedParens a -> a
getXmlBalancedParens (XmlBalancedParens a) = a

genBitWordsForever :: BS.ByteString -> Maybe (Word8, BS.ByteString)
genBitWordsForever bs = BS.uncons bs <|> Just (0, bs)
{-# INLINABLE genBitWordsForever #-}

instance FromBlankedXml (XmlBalancedParens (SimpleBalancedParens (DVS.Vector Word8))) where
  fromBlankedXml bj    = XmlBalancedParens (SimpleBalancedParens (DVS.unsafeCast (DVS.unfoldrN newLen genBitWordsForever interestBS)))
    where interestBS    = BS.concat (compressWordAsBit (blankedXmlToBalancedParens (getBlankedXml bj)))
          newLen        = (BS.length interestBS + 7) `div` 8 * 8

instance FromBlankedXml (XmlBalancedParens (SimpleBalancedParens (DVS.Vector Word16))) where
  fromBlankedXml bj    = XmlBalancedParens (SimpleBalancedParens (DVS.unsafeCast (DVS.unfoldrN newLen genBitWordsForever interestBS)))
    where interestBS    = BS.concat (compressWordAsBit (blankedXmlToBalancedParens (getBlankedXml bj)))
          newLen        = (BS.length interestBS + 7) `div` 8 * 8

instance FromBlankedXml (XmlBalancedParens (SimpleBalancedParens (DVS.Vector Word32))) where
  fromBlankedXml bj    = XmlBalancedParens (SimpleBalancedParens (DVS.unsafeCast (DVS.unfoldrN newLen genBitWordsForever interestBS)))
    where interestBS    = BS.concat (compressWordAsBit (blankedXmlToBalancedParens (getBlankedXml bj)))
          newLen        = (BS.length interestBS + 7) `div` 8 * 8

instance FromBlankedXml (XmlBalancedParens (SimpleBalancedParens (DVS.Vector Word64))) where
  fromBlankedXml bj    = XmlBalancedParens (SimpleBalancedParens (DVS.unsafeCast (DVS.unfoldrN newLen genBitWordsForever interestBS)))
    where interestBS    = BS.concat (compressWordAsBit (blankedXmlToBalancedParens (getBlankedXml bj)))
          newLen        = (BS.length interestBS + 7) `div` 8 * 8
