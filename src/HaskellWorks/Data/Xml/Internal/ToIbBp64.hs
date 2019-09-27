{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module HaskellWorks.Data.Xml.Internal.ToIbBp64
  ( toBalancedParens64
  , toInterestBits64
  , toBalancedParens64'
  , toInterestBits64'
  , toIbBp64
  ) where

import Control.Applicative
import Data.Word
import HaskellWorks.Data.Xml.Conduit
import HaskellWorks.Data.Xml.Internal.BalancedParens
import HaskellWorks.Data.Xml.Succinct.Cursor.BlankedXml   (BlankedXml (..))
import HaskellWorks.Data.Xml.Succinct.Cursor.InterestBits (blankedXmlToInterestBits, genInterestForever)

import qualified Data.ByteString      as BS
import qualified Data.Vector.Storable as DVS

genBitWordsForever :: BS.ByteString -> Maybe (Word8, BS.ByteString)
genBitWordsForever bs = BS.uncons bs <|> Just (0, bs)
{-# INLINABLE genBitWordsForever #-}

toBalancedParens64 :: BlankedXml -> DVS.Vector Word64
toBalancedParens64 (BlankedXml bj) = DVS.unsafeCast (DVS.unfoldrN newLen genBitWordsForever interestBS)
  where interestBS    = BS.concat (compressWordAsBit (blankedXmlToBalancedParens bj))
        newLen        = (BS.length interestBS + 7) `div` 8 * 8

toBalancedParens64' :: BlankedXml -> [BS.ByteString]
toBalancedParens64' (BlankedXml bj) = compressWordAsBit (blankedXmlToBalancedParens bj)

toInterestBits64 :: BlankedXml -> DVS.Vector Word64
toInterestBits64 (BlankedXml bj) = DVS.unsafeCast (DVS.unfoldrN newLen genInterestForever interestBS)
    where interestBS    = BS.concat (blankedXmlToInterestBits bj)
          newLen        = (BS.length interestBS + 7) `div` 8 * 8

toInterestBits64' :: BlankedXml -> [BS.ByteString]
toInterestBits64' (BlankedXml bj) = blankedXmlToInterestBits bj

toIbBp64 :: BlankedXml -> [(BS.ByteString, BS.ByteString)]
toIbBp64 bj = zip (toInterestBits64' bj) (toBalancedParens64' bj)
